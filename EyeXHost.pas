unit EyeXHost;

interface

(*
 * EyeXHost class: Responsible for the gaze interaction within a window.
 * Holds the current set of activatable regions and acts as a simple interactor repository.
 * Sends notifications as Windows messages so that they are received on the main thread and can be handled there.
 *
 * Copyright 2013 Tobii Technology AB. All rights reserved.
 *)

uses
  winapi.windows,System.SyncObjs, system.ansistrings, system.Classes, system.SysUtils, EyeX,
  System.types;

const
{$IFDEF WIN32}
WINDOW_HANDLE_FORMAT = '%u';
{$ENDIF}
{$IFDEF WIN64}
WINDOW_HANDLE_FORMAT = '%u';
{$ENDIF}

type

TActivatableRegion = record
  id: integer;
  bounds: TRect;
end;

TActivatableRegions = array of TActivatableRegion;

PEyeXHost =^TEyeXHost;
TEyeXHost = class(TObject)
  constructor create;
  destructor Destroy; override;
 	// attaches to the window with the given handle.
	// the message parameters are custom windows messages sent to the window when an event has occurred.
	procedure Init(Wnd: hwnd; statusChangedMessage: cardinal; focusedRegionChangedMessage: cardinal;
                 regionActivatedMessage: cardinal);
	// updates the collection (repository) of activatable regions.
	procedure SetActivatableRegions(newregions: TActivatableRegions);
	// triggers an activation ("direct click").
	procedure TriggerActivation;
private
	// attached window and custom messages.

	// mutex protecting the state of the object from race conditions caused by multiple threads.
	// (for example, a call to SetActivatableRegions from the main thread while the HandleQuery
	// method is iterating through the regions on a worker thread.)
  regions: TActivatableRegions;
	context: TX_CONTEXTHANDLE;
	connectionStateChangedTicket: TX_TICKET;
	queryHandlerTicket: TX_TICKET;
	eventHandlerTicket: TX_TICKET;


	// registers handlers for notifications from the engine.
	function RegisterConnectionStateChangedHandler: boolean;
	function RegisterQueryHandler: boolean;
	function RegisterEventHandler: boolean;

	// event handlers.
	procedure OnEngineConnectionStateChanged(connectionState: TX_CONNECTIONSTATE);
	procedure HandleQuery(hAsyncData: TX_CONSTHANDLE);
	procedure HandleEvent(hAsyncData: TX_CONSTHANDLE);
	procedure HandleActivatableEvent(hEvent: TX_HANDLE;  interactorId: Integer);
	procedure OnActivationFocusChanged(hBehavior: TX_HANDLE;  interactorId: Integer);
	procedure OnActivated(hBehavior: TX_HANDLE;  interactorId: Integer);

  function QueryIsForWindowId(hQuery: TX_HANDLE; windowId: pansichar): boolean;
  public
	texhWnd: HWND;
	texhstatusChangedMessage: cardinal;
	texhfocusedRegionChangedMessage: cardinal;
	texhregionActivatedMessage: cardinal;
end;

procedure cb_OnEngineConnectionStateChanged(connectionState: TX_CONNECTIONSTATE; UserParam: TX_UserParam ); {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}
procedure cb_HandleQuery(hAsyncData: TX_CONSTHANDLE;  userParam: TX_USERPARAM); {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}
procedure cb_HandleEvent(hAsyncData: TX_CONSTHANDLE;  userParam: TX_USERPARAM); {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}
	// callback function invoked when a snapshot has been committed.
procedure cb_OnSnapshotCommitted(hAsyncData: TX_CONSTHANDLE; param: TX_USERPARAM); {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


const
 nullptr = nil;

var   mutex: TMutex;


implementation

procedure cb_OnEngineConnectionStateChanged(connectionState: TX_CONNECTIONSTATE; UserParam: TX_UserParam );
begin
  TEyeXHost(userparam).OnEngineConnectionStateChanged(connectionState);
end;
procedure cb_HandleQuery(hAsyncData: TX_CONSTHANDLE;  userParam: TX_USERPARAM);
begin
  TEyeXHost(UserParam).handlequery(hAsyncData);
end;
procedure cb_HandleEvent(hAsyncData: TX_CONSTHANDLE;  userParam: TX_USERPARAM);
begin
  TEyeXHost(UserParam).handleEvent(hAsyncData);
end;

constructor TEyeXHost.create;

begin
 inherited;
 texhWnd := 0;
 texhstatusChangedMessage := 0;
 texhfocusedRegionChangedMessage := 0;
 texhregionActivatedMessage := 0;

	// initialize the EyeX Engine client library.
	txInitializeEyeX(TX_EYEXCOMPONENTOVERRIDEFLAG_NONE, nullptr, nullptr, nullptr, nullptr);

  //create Mutex here...
  mutex := TMutex.create(false);
end;


destructor TEyeXHost.destroy ;

begin
	if (context <> TX_EMPTY_HANDLE) then
	begin
		// shut down, then release the context.
		txShutdownContext(context, TX_CLEANUPTIMEOUT_DEFAULT, TX_FALSE);
		txReleaseContext(context);
	end;
  //destroy Mutex here...
  mutex.free;
  inherited;
end;

procedure  TEyeXHost.Init(Wnd: HWND;  statusChangedMessage: UINT;  focusedRegionChangedMessage: UINT;  regionActivatedMessage: UINT);
begin
	texhWnd := Wnd;
	texhstatusChangedMessage := statusChangedMessage;
	texhfocusedRegionChangedMessage := focusedRegionChangedMessage;
	texhregionActivatedMessage := regionActivatedMessage;

	// create a context and register event handlers.
	txCreateContext(context, TX_FALSE);
 	RegisterConnectionStateChangedHandler;
	RegisterQueryHandler;
	RegisterEventHandler;

	// connect to the engine.
	if (txEnableConnection(context) <> TX_RESULT_OK) then
		PostMessage(texhWnd, texhstatusChangedMessage, tx_false, 0);
end;

procedure TEyeXHost.SetActivatableRegions(newregions: TActivatableRegions);
begin
  mutex.Acquire;
  try
  setlength(regions,0);
 	regions := copy(newregions);
  finally
    mutex.release;
  end;
end;


procedure TEyeXHost.TriggerActivation;

var command: TX_Handle;

begin
  command := TX_EMPTY_HANDLE;
	txCreateActionCommand(context, command, TX_ACTIONTYPE_ACTIVATE);
	txExecuteCommandAsync(command, nil, nil);
	txReleaseObject(command);
end;


procedure TEyeXHost.OnEngineConnectionStateChanged(connectionState: TX_CONNECTIONSTATE);
begin
	// note the use of the asynchronous PostMessage function to marshal the event to the main thread.
	// (this callback function is typically invoked on a worker thread.)
	case (connectionState) of
  	TX_CONNECTIONSTATE_CONNECTED:  PostMessage(texhWnd, texhstatusChangedMessage, tx_true, 0);
    TX_CONNECTIONSTATE_DISCONNECTED,TX_CONNECTIONSTATE_TRYINGTOCONNECT
	  ,TX_CONNECTIONSTATE_SERVERVERSIONTOOLOW,TX_CONNECTIONSTATE_SERVERVERSIONTOOHIGH:
		PostMessage(texhWnd, texhstatusChangedMessage, tx_false, 0);
  end;
end;

function TEyeXHost.RegisterConnectionStateChangedHandler: boolean;
begin
	result := txRegisterConnectionStateChangedHandler(context, connectionStateChangedTicket, cb_OnEngineConnectionStateChanged, self) = TX_RESULT_OK;
end;

function TEyeXHost.RegisterQueryHandler;
begin
	result := txRegisterQueryHandler(context, queryHandlerTicket, cb_HandleQuery, self) = TX_RESULT_OK;
end;

function TEyeXHost.RegisterEventHandler;
begin
	result := txRegisterEventHandler(context, eventHandlerTicket, cb_HandleEvent, self) = TX_RESULT_OK;
end;

procedure TEyeXHost.HandleQuery(hAsyncData: TX_CONSTHANDLE);

const bufferSize = 20;

var   hQuery, hbounds, hsnapshot:TX_HANDLE;
      StringBuffer: ansistring;
      WindowIdString: ansistring;
      pX,pY, pWidth, pHeight: TX_REAL;
		  params: TX_ACTIVATABLEPARAMS;
      QueryBounds: TRect;
      region: TActivatableRegion;
      hInteractor: TX_HANDLE;
      bounds: TX_RECT;
      success: boolean;

begin
  hquery := TX_EMPTY_HANDLE;
  hbounds := TX_EMPTY_HANDLE;
  hsnapshot := TX_EMPTY_HANDLE;
  mutex.Acquire;

  try
	// NOTE. This method will fail silently if, for example, the connection is lost before the snapshot has been committed,
	// or if we run out of memory. This is by design, because there is nothing we can do to recover from these errors anyway.
	success := txGetAsyncDataContent(hAsyncData, hQuery) = tx_result_ok;
	// read the query bounds from the query, that is, the area on the screen that the query concerns.
	// the query region is always rectangular.
	success := success and (txGetQueryBounds(hQuery, hBounds) = tx_result_ok);
  success := success and (txGetRectangularBoundsData(hBounds, pX, pY, pWidth, pHeight) = tx_result_ok);
	success := success and (txReleaseObject(hBounds) = tx_result_ok);

	queryBounds := TRect.Create(point(round(pX),round(pY)),round(pWidth),round(pHeight));

	// create a new snapshot with the same window id and bounds as the query.
	success := success and (txCreateSnapshotForQuery(hQuery, hSnapshot) = tx_result_ok);
  //Writes windowhandle to string
	WindowIdString := system.ansistrings.format(WINDOW_HANDLE_FORMAT, [texhWnd]);

	if (QueryIsForWindowId(hQuery, PAnsiChar(windowIdString))) then
	begin
		// define options for our activatable regions: yes, we want tentative focus events.
    params.EnableTentativeFocus  := TX_TRUE;
    params.EnableSmallItemDetection := TX_False;
    // iterate through all regions and create interactors for those that overlap with the query bounds.
		for region in regions do
		begin
      if queryBounds.IntersectsWith(region.Bounds) then
			begin
				hInteractor := TX_EMPTY_HANDLE;
				stringBuffer := system.ansistrings.format('%u', [region.id]);

				bounds.X := region.bounds.left;
				bounds.Y := region.bounds.top;
				bounds.Width := region.bounds.Width;
				bounds.Height := region.bounds.Height;

				success := success and (txCreateRectangularInteractor(hSnapshot, hInteractor, PAnsiChar(stringBuffer), bounds, TX_LITERAL_ROOTID,  PAnsiChar(windowIdString)) = tx_result_ok);
				success := success and (txCreateActivatableBehavior(hInteractor, params) = tx_result_ok);
				success := success and (txReleaseObject(hInteractor) = tx_result_ok);
			end;
		end;
	end;

	txCommitSnapshotAsync(hSnapshot, cb_OnSnapshotCommitted, nullptr);
	txReleaseObject(hSnapshot);
	txReleaseObject(hQuery);

  finally
    mutex.release;
  end;
end;

procedure TEyeXHost.HandleEvent(hAsyncData: TX_CONSTHANDLE);

const
  bufferSize = 20;

var
   hEvent: TX_HANDLE;
   idLength: TX_SIZE;
   stringBuffer: array [0..bufferSize - 1] of TX_CHAR;
   interactorId: Integer;

begin
  hEvent := TX_EMPTY_HANDLE;
  idLength := bufferSize;

	txGetAsyncDataContent(hAsyncData, hEvent);

	// NOTE. Uncomment the following line of code to view the event object. The same function can be used with any interaction object.
	//OutputDebugStringA(txDebugObject(hEvent));

	// read the interactor ID from the event.
	if (txGetEventInteractorId(hEvent, PAnsichar(@stringBuffer), idLength) = TX_RESULT_OK) then
	begin
		interactorId := strtoint(string(stringBuffer));
		HandleActivatableEvent(hEvent, interactorId);
	end;

	txReleaseObject(hEvent);
end;

procedure TEyeXHost.HandleActivatableEvent(hEvent: TX_HANDLE;  interactorId: Integer);

var 		eventType: TX_ACTIVATABLEEVENTTYPE;
        hActivatable: TX_HANDLE;

begin
  hActivatable := TX_EMPTY_HANDLE;
	if (txGetEventBehavior(hEvent, hActivatable, TX_BEHAVIORTYPE_ACTIVATABLE) = TX_RESULT_OK) then
	begin
		if (txGetActivatableEventType(hActivatable, eventType) = TX_RESULT_OK) then
		begin
			if (eventType = TX_ACTIVATABLEEVENTTYPE_ACTIVATED) then
				OnActivated(hActivatable, interactorId)
			else if (eventType = TX_ACTIVATABLEEVENTTYPE_ACTIVATIONFOCUSCHANGED) then
				OnActivationFocusChanged(hActivatable, interactorId);
		end;
		txReleaseObject(hActivatable);
	end;
end;

procedure TEyeXHost.OnActivationFocusChanged(hBehavior: TX_HANDLE;  interactorId: Integer);

var 	eventData: TX_ACTIVATIONFOCUSCHANGEDEVENTPARAMS;

begin
	if (txGetActivationFocusChangedEventParams(hBehavior, eventData) = TX_RESULT_OK) then
		if (eventData.HasActivationFocus = tx_true) or (eventData.HasTentativeActivationFocus = tx_true)then
			PostMessage(texhWnd, texhfocusedRegionChangedMessage, interactorId, 0)
		else
		  PostMessage(texhWnd, texhfocusedRegionChangedMessage, UINT_PTR(-1), 0);
end;

procedure TEyeXHost.OnActivated(hBehavior: TX_HANDLE;  interactorId: Integer);
begin
	PostMessage(texhWnd, texhregionActivatedMessage, interactorId, 0);
end;

procedure cb_OnSnapshotCommitted(hAsyncData: TX_CONSTHANDLE;  param: TX_USERPARAM);

var 	result: TX_RESULT ;

begin
	// check the result code using an assertion.
	// this will catch validation errors and runtime errors in debug builds. in release builds it won't do anything.
  result := TX_RESULT_UNKNOWN;
  txGetAsyncDataResultCode(hAsyncData, result);
	assert((result = TX_RESULT_OK) or (result = TX_RESULT_CANCELLED));
end;

function TEyeXHost.QueryIsForWindowId(hQuery: TX_HANDLE;  windowId: PANSICHAR): boolean;

var 	size,count: TX_SIZE;
      buffer: Array[0..19] of ansichar;
      i: TX_integer;

begin
  Size := 20;
	if (TX_RESULT_OK = txGetQueryWindowIdCount(hQuery, count)) then
	begin
		for i:= 0 to count-1 do
			if (TX_RESULT_OK = txGetQueryWindowId(hQuery, i, pansichar(@buffer), size)) then
				if (windowId = ansistring(buffer)) then
        begin
				 result := true;
         exit;
        end;
	end;
	result := false;
end;


end.
