 (*********************************************************************************************************************
 * Delphi header file for EyeX v1.0
 * Converted from the original C header files (Copyright 2013-2014 Tobii Technology AB, all rights reserved)
 * by Alan McNicol, Ealanach Ltd
 *********************************************************************************************************************)

 //Compliler directives - 64bit code works only if Z4 is set (32 bit also works).
 //The appropriate value value for $A (align) has not yet been determined
 {$A4,Z4}

 unit EyeX;

interface


const
  TX_ENUM_STARTVALUE                  = 1;
  TX_INTERNAL_ENUM_STARTVALUE         = 10000000;
  TX_FLAGS_NONE_VALUE                 = 0;


//If the eyeX dll is in the same folder as the exe file or is in the windows/system32 folder for the 64-bit dll and/or the windows/sysWOW64 folder for
//the 32-bit dll then no path is required and the below will function.  If you wish the program to load
//the dll from a specific folder then add paths to the eyeXdll values below as appropriate.
{$IFDEF WIN32}
  eyeXdll = 'Tobii.EyeX.Client.dll';
{$ENDIF}
{$IFDEF WIN64}
  eyeXdll = 'Tobii.EyeX.Client.dll';
{$ENDIF}

(*********************************************************************************************************************
 * EyeXLiterals.h
 *********************************************************************************************************************)

(*********************************************************************************************************************
 * Literals
 *********************************************************************************************************************)
   (**
    *   Message Literals
    *)
	 const TX_LITERAL_HEADER: PAnsiChar ='Header';
	 const TX_LITERAL_BODY: PAnsiChar ='Body';
	 const TX_LITERAL_ID: PAnsiChar ='Id';
	 const TX_LITERAL_PROCESSID: PAnsiChar ='ProcessId';

	(**
	*    Client Literals
	*)
	 const TX_LITERAL_AGENTID: PAnsiChar ='AgentId';
	 const TX_LITERAL_TARGETPROCESSID: PAnsiChar ='TargetProcessId';
	 const TX_LITERAL_CLIENTMODE: PAnsiChar ='ClientMode';

    (**
    *   Miscellaneous Literals
    *)
	 const TX_LITERAL_TYPE: PAnsiChar ='Type';
	 const TX_LITERAL_TIMESTAMP: PAnsiChar ='Timestamp';
	 const TX_LITERAL_DATA: PAnsiChar ='Data';
	 const TX_LITERAL_PARAMETERS: PAnsiChar = 'Parameters';
	 const TX_LITERAL_X: PAnsiChar = 'X';
	 const TX_LITERAL_Y: PAnsiChar = 'Y';
	 const TX_LITERAL_Z: PAnsiChar = 'Z';

	(**
	*   Bounds Literals
    *)
	 const TX_LITERAL_BOUNDS: PAnsiChar =  'Bounds';
	 const TX_LITERAL_BOUNDSTYPE: PAnsiChar = 'BoundsType';
	 const TX_LITERAL_NONE: PAnsiChar = 'None';
	 const TX_LITERAL_RECTANGULAR: PAnsiChar = 'Rectangular';
	 const TX_LITERAL_TOP: PAnsiChar = 'Top';
	 const TX_LITERAL_LEFT: PAnsiChar = 'Left';
	 const TX_LITERAL_RIGHT: PAnsiChar = 'Right';
	 const TX_LITERAL_BOTTOM: PAnsiChar = 'Bottom';
	 const TX_LITERAL_WIDTH: PAnsiChar = 'Width';
	 const TX_LITERAL_HEIGHT: PAnsiChar = 'Height';

    (**
    *   Interactor Literals
    *)
	 const TX_LITERAL_ROOTID: PAnsiChar = '_RootId';
	 const TX_LITERAL_GLOBALINTERACTORWINDOWID: PAnsiChar = 'GlobalInteractorWindowId';
	 const TX_LITERAL_MASK: PAnsiChar = 'Mask';
	 const TX_LITERAL_MASKID: PAnsiChar = 'MaskId';
	 const TX_LITERAL_MASKBOUNDS: PAnsiChar = 'MaskBounds';

    (**
    *   Mask Literals
    *)
	 const TX_LITERAL_MASKTYPE: PAnsiChar = 'MaskType';
	 const TX_LITERAL_ROWCOUNT: PAnsiChar = 'RowCount';
	 const TX_LITERAL_COLUMNCOUNT: PAnsiChar = 'ColumnCount';

    (**
    * Gaze Point Data Behavior Literals
    *)
	 const TX_LITERAL_GAZEPOINTDATAMODE: PAnsiChar = 'GazePointDataMode';
	 const TX_LITERAL_GAZEPOINTDATAEVENTTYPE: PAnsiChar = 'GazePointDataEventType';

    (**
    *    Activation Behavior Literals
    *)
	 const TX_LITERAL_ACTIVATABLEEVENTTYPE: PAnsiChar = 'ActivatableEventType';
	 const TX_LITERAL_HASACTIVATIONFOCUS: PAnsiChar = 'HasActivationFocus';
	 const TX_LITERAL_HASTENTATIVEACTIVATIONFOCUS: PAnsiChar = 'HasTentativeActivationFocus';
	 const TX_LITERAL_ISACTIVATED: PAnsiChar = 'IsActivated';
	 const TX_LITERAL_ISTENTATIVEFOCUSENABLED: PAnsiChar = 'IsTentativeFocusEnabled';
	 const TX_LITERAL_ISSMALLITEMDETECTIONENABLED: PAnsiChar = 'IsSmallItemDetectionEnabled';

    (**
    * Fixation Data Behavior Literals
    *)
	 const TX_LITERAL_FIXATIONDATAMODE: PAnsiChar = 'FixationDataMode';
	 const TX_LITERAL_FIXATIONDATAEVENTTYPE: PAnsiChar = 'FixationDataEventType';

    (**
    * Action data Behavior Literals
    *)
	 const TX_LITERAL_ACTIONDATAEVENTTYPE: PAnsiChar = 'ActionDataEventType';
	 const TX_LITERAL_ACTIVATIONMISSED: PAnsiChar = 'ActivationMissed';

    (**
    * Gaze-Aware Behavior Literals
    *)
	 const TX_LITERAL_HASGAZE: PAnsiChar = 'HasGaze';
	 const TX_LITERAL_GAZEAWAREMODE: PAnsiChar = 'GazeAwareMode';
	 const TX_LITERAL_DELAYTIME: PAnsiChar = 'DelayTime';

    (**
    * Gaze Data Diagnostics Behavior Literals
    *)
	 const TX_LITERAL_QUALITY: PAnsiChar = 'Quality';
	 const TX_LITERAL_NOISE: PAnsiChar = 'Noise';
	 const TX_LITERAL_INSACCADE: PAnsiChar = 'InSaccade';
	 const TX_LITERAL_INFIXATION: PAnsiChar = 'InFixation';

    (**
    * Eye Position Behavior Literals
    *)
	 const TX_LITERAL_LEFTEYEPOSITION: PAnsiChar = 'LeftEyePosition';
	 const TX_LITERAL_RIGHTEYEPOSITION: PAnsiChar = 'RightEyePosition';
	 const TX_LITERAL_LEFTEYEPOSITIONNORMALIZED: PAnsiChar = 'LeftEyePositionNormalized';
	 const TX_LITERAL_RIGHTEYEPOSITIONNORMALIZED: PAnsiChar = 'RightEyePositionNormalized';
	 const TX_LITERAL_HASLEFTEYEPOSITION: PAnsiChar = 'HasLeftEyePosition';
	 const TX_LITERAL_HASRIGHTEYEPOSITION: PAnsiChar = 'HasRightEyePosition';

    (**
    * Presence Behavior Literals
    *)
	 const TX_LITERAL_PRESENCEDATA: PAnsiChar = 'Presence';


    (**
    * Pannable Behavior Literals
    *)
	 const TX_LITERAL_PANVELOCITYX: PAnsiChar = 'PanVelocityX';
	 const TX_LITERAL_PANVELOCITYY: PAnsiChar = 'PanVelocityY';
	 const TX_LITERAL_PANSTEPX: PAnsiChar = 'PanStepX';
	 const TX_LITERAL_PANSTEPY: PAnsiChar = 'PanStepY';
	 const TX_LITERAL_PANSTEPDURATION: PAnsiChar = 'PanStepDuration';
	 const TX_LITERAL_PANHANDSFREE: PAnsiChar = 'PanHandsFree';
	 const TX_LITERAL_PANPROFILE: PAnsiChar = 'Profile';
	 const TX_LITERAL_PANDIRECTIONSAVAILABLE: PAnsiChar = 'PanDirectionsAvailable';
	 const TX_LITERAL_PANPEAKVELOCITY: PAnsiChar = 'PeakVelocity';
	 const TX_LITERAL_PANADAPTVELOCITYTOVIEWPORT: PAnsiChar = 'AdaptVelocityToViewport';
	 const TX_LITERAL_PANMAXZONERELATIVESIZE: PAnsiChar = 'MaxPanZoneRelativeSize';
	 const TX_LITERAL_PANMAXZONESIZE: PAnsiChar = 'MaxPanZoneSize';
	 const TX_LITERAL_PANZONESIZE: PAnsiChar = 'PanZoneSize';
	 const TX_LITERAL_PANNABLEEVENTTYPE: PAnsiChar = 'PannableEventType';

    (**
    *   Callback Response Literals
    *)
	 const TX_LITERAL_REQUESTTYPE: PAnsiChar = 'RequestType';
	 const TX_LITERAL_REQUESTID: PAnsiChar = 'RequestId';
	 const TX_LITERAL_ERRORMESSAGE: PAnsiChar = 'ErrorMessage';
	 const TX_LITERAL_RESULT: PAnsiChar = 'Result';

    (**
    *   Interaction Mode Literals
    *)
	 const TX_LITERAL_ACTIONTYPE: PAnsiChar = 'ActionType';

    (**
    *   State literals
    *)
	 const TX_LITERAL_STATEPATH: PAnsiChar = 'StatePath';
	 const TX_LITERAL_STATEPATHDELIMITER: PAnsiChar = '.';

    (*
    *  Configuration Tool Literals
    *)
	 const TX_LITERAL_CONFIGURATIONTOOL: PAnsiChar = 'ConfigurationTool';

    (*
    *  Current Profile Literals
    *)
	 const TX_LITERAL_PROFILENAME: PAnsiChar = 'ProfileName';



(*********************************************************************************************************************)

(**
 * Literals for state paths.
 *
 * @field TX_STATEPATH_EYETRACKING:
 *   The root node for all eyetracking information.
 *   GETTABLE.
 *
 * @field TX_STATEPATH_EYETRACKINGSCREENBOUNDS:
 *   Holds the virtual screen bounds in pixels.
 *   The value can be retrieved from the state bag as a TX_RECT structure with GetStateValueAsRectangle.
 *   If the screen bounds can not be determined screen bounds (0, 0, 0, 0) will be returned.
 *   Replaces deprecated state path TX_STATEPATH_SCREENBOUNDS from version 1.3.0.
 *   GETTABLE.
 *  \since Version 1.3.0
 *
 * @field TX_STATEPATH_EYETRACKINGDISPLAYSIZE:
 *   Holds the display size in millimeters as width and height.
 *   The value can be retrieved from the state bag as a TX_SIZE2 structure with GetStateValueAsSize2.
 *   If the display size can not be determined Width and Height (0, 0) will be returned.
 *   Replaces deprecated state path TX_STATEPATH_DISPLAYSIZE from version 1.3.0.
 *   GETTABLE.
 *  \since Version 1.3.0
 *
 * @field TX_STATEPATH_EYETRACKINGSTATE:
 *   Holds the eye tracking state. The value is of type TX_EYETRACKINGDEVICESTATUS.
 *   GETTABLE.
 *
 * @field TX_STATEPATH_EYETRACKINGCURRENTPROFILE:
 *   Holds the following data:
 *   'name' - See TX_STATEPATH_EYETRACKINGCURRENTPROFILENAME.
 *   'trackedeyes' - See TX_STATEPATH_EYETRACKINGCURRENTPROFILETRACKEDEYES.
  *   GETTABLE.
 *  \since Version 1.3.0
 *
 * @field TX_STATEPATH_EYETRACKINGCURRENTPROFILENAME:
 *   Holds the name of the current eye tracking profile. The value is of type TX_STRING.
 *   Replaces deprecated state path TX_STATEPATH_PROFILENAME from version 1.3.0.
 *   GETTABLE.
 *  \since Version 1.3.0
 *
 * @field TX_STATEPATH_EYETRACKINGCURRENTPROFILETRACKEDEYES:
 *   Holds the tracked eyes of the current eye tracking profile. The value is of type TX_TRACKEDEYES.
 *   GETTABLE and SETTABLE.
 *  \since Version 1.3.0
 *
 * @field TX_STATEPATH_EYETRACKINGPROFILES:
 *   Holds the list of available eye tracking profiles. The value is an array of TX_STRING. It can be accessed with
 *   txGetStateValueAsString as a string containing all profiles separated with a null termination character.
 *   There is also a utility function available to access profiles as a std::vector of std::strings, see Tx::GetStateValueAsArrayOfStrings.
 *   GETTABLE.
 *  \since Version 1.3.0
 *
 * @field TX_STATEPATH_EYETRACKINGCONFIGURATIONSTATUS:
 *   Holds the configuration status of the eye tracker. The value is of type TX_EYETRACKINGCONFIGURATIONSTATUS.
 *   GETTABLE.
 *   \since Version 1.1.0
 *
 * @field TX_STATEPATH_EYETRACKINGINFO:
 *   Holds information about the eye tracker.
 *   GETTABLE.
 *   \since Version 1.3.0
 *
 * @field TX_STATEPATH_EYETRACKINGINFOMODELNAME:
 *   Eye tracker model name. The value is of type TX_STRING.
 *   GETTABLE.
 *   \since Version 1.3.0
 *
 * @field TX_STATEPATH_EYETRACKINGINFOSERIALNUMBER:
 *   Eye tracker serial number. The value is of type TX_STRING.
 *   GETTABLE.
 *   \since Version 1.3.0
 *
 * @field TX_STATEPATH_EYETRACKINGINFOGENERATION:
 *   Eye tracker generation name. The value is of type TX_STRING.
 *   GETTABLE.
 *   \since Version 1.3.0
 *
 * @field TX_STATEPATH_EYETRACKINGINFOFIRMWAREVERSION:
 *   Eye tracker firmware version. The value is of type TX_STRING.
 *   GETTABLE.
 *   \since Version 1.3.0
 *
 * @field TX_STATEPATH_EYETRACKINGINFOEYEXCONTROLLERCOREVERSION:
 *   EyeX Controller Core Version. The value is of type TX_STRING.
 *   GETTABLE.
 *   \since Version 1.3.0
 *
 * @field TX_STATEPATH_ENGINEINFOVERSION:
 *   Reports the engine version. The value is of type TX_STRING.
 *   Replaces deprecated state path TX_STATEPATH_ENGINEVERSION since version 1.3.0.
 *   GETTABLE.
 *   \since Version 1.3.0
 *
 * @field TX_STATEPATH_ENGINEINFOINTERNAL:
 *   Holds the following data:
 *   'tobiiserviceversion' - The installed version of Tobii Service as TX_STRING or empty string if not installed.
 *   'tobiieyexcontrollerdriverversion' - The installed version of Tobii EyeX Controller Driver as TX_STRING or empty string if not installed.
 *   'tobiiusbserviceversion' - The installed version of Tobii USB Service version as TX_STRING or empty string if not installed.
 *   'tobiieyexinteractionversion' - The installed version of Tobii EyeX Interaction as TX_STRING or empty string if not installed.
 *   'tobiieyexconfigversion' - The installed version of Tobii EyeX Config as TX_STRING or empty string if not installed.
 *   GETTABLE.
 *   \since Version 1.3.0
 *
 * @field TX_STATEPATH_USERPRESENCE:
 *   Holds data about user presence. The value is of type TX_USERPRESENCE.
 *   The value of TX_USERPRESENCE will be TX_USERPRESENCE_UNKNOWN if there isn't any observer registered for this state.
 *   GETTABLE.
 *
 * @field TX_STATEPATH_FAILEDACTION:
 *   Notifies when interactions fail. The value is of type TX_FAILEDACTIONTYPE.
 *   SUBSCRIBABLE.
 *
 * @field TX_STATEPATH_INTERACTIONMODES:
 *   Holds the current engine interaction mode. The value is of type TX_INTERACTIONMODES.
 *   GETTABLE.
 *   \since Version 1.1.0
 *
 * @field TX_STATEPATH_PROFILENAME:
 *   Holds the name of the eye tracking profile used. The value is of type TX_STRING.
 *   Deprecated, use TX_STATEPATH_EYETRACKINGCURRENTPROFILENAME for engine version 1.3.0 and greater.
 *   GETTABLE.
 *
 * @field TX_STATEPATH_ENGINEVERSION:
 *   Reports the engine version. The value is of type TX_STRING.
 *   Deprecated, use TX_STATEPATH_ENGINEINFOVERSION for engine version 1.3.0 and greater.
 *   GETTABLE.
 *
 * @field TX_STATEPATH_SCREENBOUNDS:
 *   Holds the virtual screen bounds in pixels.
 *   The value can be retrieved from the state bag as a TX_RECT structure with GetStateValueAsRectangle.
 *   If the screen bounds can not be determined screen bounds (0, 0, 0, 0) will be returned.
 *   Deprecated, use TX_STATEPATH_EYETRACKINGSCREENBOUNDS for engine version 1.3.0 and greater.
 *   GETTABLE.
 *
 * @field TX_STATEPATH_DISPLAYSIZE:
 *   Holds the display size in millimeters as width and height.
 *   The value can be retrieved from the state bag as a TX_SIZE2 structure with GetStateValueAsSize2.
 *   If the display size can not be determined Width and Height (0, 0) will be returned.
 *   Deprecated, use TX_STATEPATH_EYETRACKINGDISPLAYSIZE for engine version 1.3.0 and greater.
 *   GETTABLE.
 *
 * @field TX_STATEPATH_CONFIGURATIONSTATUS:
 *   Holds the configuration status of the eye tracker. The value is of type TX_EYETRACKINGCONFIGURATIONSTATUS.
 *   \since Version 1.1.0
 *   Deprecated, use TX_STATEPATH_EYETRACKINGCONFIGURATIONSTATUS for engine version 1.3.0 and greater.
 *   GETTABLE.
 *
 *)

	 const
	   /// <summary>
	   ///   The root node for all eyetracking information.
	   /// </summary>
	   TX_STATEPATH_EYETRACKING: PAnsiChar = 'eyeTracking';
	 const
	   /// <summary>
	   ///   Holds the virtual screen bounds in pixels. <br />The value can be
	   ///   retrieved from the state bag as a TX_RECT structure with
	   ///   GetStateValueAsRectangle. <br />If the screen bounds can not be
	   ///   determined screen bounds (0, 0, 0, 0) will be returned. <br /><br />
	   /// </summary>
	   /// <remarks>
	   ///   Replaces deprecated state path TX_STATEPATH_SCREENBOUNDS from version
	   ///   1.3.0.
	   /// </remarks>
	   TX_STATEPATH_EYETRACKINGSCREENBOUNDS: PAnsiChar = 'eyeTracking.screenBounds';
	 const
	   /// <summary>
	   ///   Holds the display size in millimeters as width and height. <br />The
	   ///   value can be retrieved from the state bag as a TX_SIZE2 structure
	   ///   with GetStateValueAsSize2. <br />If the display size can not be
	   ///   determined Width and Height (0, 0) will be returned. <br />
	   /// </summary>
	   /// <remarks>
	   ///   Replaces deprecated state path TX_STATEPATH_DISPLAYSIZE from version
	   ///   1.3.0.
	   /// </remarks>
	   TX_STATEPATH_EYETRACKINGDISPLAYSIZE: PAnsiChar = 'eyeTracking.displaySize';
	 const
	   /// <summary>
	   ///   Holds the eye tracking state. The value is of type
	   ///   TX_EYETRACKINGDEVICESTATUS.
	   /// </summary>
	   TX_STATEPATH_EYETRACKINGSTATE: PAnsiChar = 'eyeTracking.state';

	 const
	   /// <summary>
	   ///   Holds the list of available eye tracking profiles. The value is an
	   ///   array of TX_STRING. It can be accessed with TxGetStateValueAsString
	   ///   as a string containing all profiles separated with a null termination
	   ///   character. <br />There is also a utility function available to access
	   ///   profiles as a std::vector of std::strings, see
	   ///   TxGetStateValueAsArrayOfStrings. <br />
	   /// </summary>
	   TX_STATEPATH_EYETRACKINGPROFILES: PAnsiChar = 'eyeTracking.profiles';
	 const
	   /// <summary>
	   ///   Holds the name of the eye tracking profile used. The value is of type
	   ///   TX_STRING. <br />Deprecated, use
	   ///   TX_STATEPATH_EYETRACKINGCURRENTPROFILENAME for engine version 1.3.0
	   ///   and greater. <br />
	   /// </summary>
	   TX_STATEPATH_EYETRACKINGCURRENTPROFILE: PAnsiChar = 'eyeTracking.currentprofile';
	 const
	   /// <summary>
	   ///   Holds the name of the current eye tracking profile. The value is of
	   ///   type TX_STRING. <br />Replaces deprecated state path
	   ///   TX_STATEPATH_PROFILENAME from version 1.3.0.
	   /// </summary>
	   TX_STATEPATH_EYETRACKINGCURRENTPROFILENAME: PAnsiChar = 'eyeTracking.currentprofile.name';
	 const
	   /// <summary>
	   ///   Holds the tracked eyes of the current eye tracking profile. The value
	   ///   is of type TX_TRACKEDEYES.
	   /// </summary>
	   TX_STATEPATH_EYETRACKINGCURRENTPROFILETRACKEDEYES: PAnsiChar = 'eyeTracking.currentprofile.trackedeyes';

	 const
	   /// <summary>
	   ///   Holds the configuration status of the eye tracker. The value is of
	   ///   type TX_EYETRACKINGCONFIGURATIONSTATUS. <br />
	   /// </summary>
	   TX_STATEPATH_EYETRACKINGCONFIGURATIONSTATUS: PAnsiChar = 'eyeTracking.configurationStatus';

	 const
	   /// <summary>
	   ///   EyeX Controller Core Version. The value is of type TX_STRING.
	   /// </summary>
	   TX_STATEPATH_EYETRACKINGINFO: PAnsiChar = 'eyeTracking.info';
	 const
	   /// <summary>
	   ///   Eye tracker model name. The value is of type TX_STRING.
	   /// </summary>
	   TX_STATEPATH_EYETRACKINGINFOMODELNAME: PAnsiChar = 'eyeTracking.info.modelname';
	 const
	   /// <summary>
	   ///   Eye tracker serial number. The value is of type TX_STRING.
	   /// </summary>
	   TX_STATEPATH_EYETRACKINGINFOSERIALNUMBER: PAnsiChar = 'eyeTracking.info.serialnumber';
	 const
	   /// <summary>
	   ///   Eye tracker generation name. The value is of type TX_STRING.
	   /// </summary>
	   TX_STATEPATH_EYETRACKINGINFOGENERATION: PAnsiChar = 'eyeTracking.info.generation';
	 const
	   /// <summary>
	   ///   Eye tracker firmware version. The value is of type TX_STRING.
	   /// </summary>
	   TX_STATEPATH_EYETRACKINGINFOFIRMWAREVERSION: PAnsiChar = 'eyeTracking.info.firmwareversion';
	 const TX_STATEPATH_EYETRACKINGINFOHASFIXEDDISPLAYAREA: PAnsiChar = 'eyeTracking.info.hasfixeddisplayarea';
	 const TX_STATEPATH_EYETRACKINGINFOTOBIIEYEXCONTROLLERCOREVERSION: PAnsiChar = 'eyeTracking.info.tobiieyexcontrollercoreversion';

	 const
	   /// <summary>
	   ///   Reports the engine version. The value is of type TX_STRING. <br />
	   ///   Replaces deprecated state path TX_STATEPATH_ENGINEVERSION since
	   ///   version 1.3.0.
	   /// </summary>
	   TX_STATEPATH_ENGINEINFOVERSION: PAnsiChar = 'engine.info.version';
	 const
	   /// <summary>
	   ///   Holds the following data: <br />* 'tobiiserviceversion' - The
	   ///   installed version of Tobii Service as TX_STRING or empty string if
	   ///   not installed. <br />* 'tobiieyexcontrollerdriverversion' - The
	   ///   installed version of Tobii EyeX Controller Driver as TX_STRING or
	   ///   empty string if not installed. <br />* 'tobiiusbserviceversion' - The
	   ///   installed version of Tobii USB Service version as TX_STRING or empty
	   ///   string if not installed. <br />* 'tobiieyexinteractionversion' - The
	   ///   installed version of Tobii EyeX Interaction as TX_STRING or empty
	   ///   string if not installed. <br />* 'tobiieyexconfigversion' - The
	   ///   installed version of Tobii EyeX Config as TX_STRING or empty string
	   ///   if not installed. <br />
	   /// </summary>
	   TX_STATEPATH_ENGINEINFOINTERNAL: PAnsiChar = 'engine.info.internal';

	 const TX_STATEPATH_GAZETRACKING: PAnsiChar = 'status.gazeTracking';

	 const
	   /// <summary>
	   ///   Holds data about user presence. The value is of type TX_USERPRESENCE.
	   ///   <br />The value of TX_USERPRESENCE will be TX_USERPRESENCE_UNKNOWN if
	   ///   there isn't any observer registered for this state. <br />
	   /// </summary>
	   TX_STATEPATH_USERPRESENCE: PAnsiChar = 'userPresence';

   const
     /// <summary>
     ///   Notifies when interactions fail. The value is of type
     ///   TX_FAILEDACTIONTYPE.
     /// </summary>
     TX_STATEPATH_FAILEDACTION: PAnsiChar = 'failedAction';

   const
     /// <summary>
     ///   Holds the current engine interaction mode. The value is of type
     ///   TX_INTERACTIONMODES.
     /// </summary>
     TX_STATEPATH_INTERACTIONMODES: PAnsiChar = 'status.interaction.interactionModes';

	(* Deprecated since version 1.3.0. For compatibility between client libs and engine version 1.2.1 or lesser *)
	 const
	   /// <summary>
	   ///   Holds the name of the eye tracking profile used. The value is of type
	   ///   TX_STRING. <br />Deprecated, use
	   ///   TX_STATEPATH_EYETRACKINGCURRENTPROFILENAME for engine version 1.3.0
	   ///   and greater.
	   /// </summary>
	   TX_STATEPATH_PROFILENAME: PAnsiChar = 'eyeTracking.profileName';
	 const
	   /// <summary>
	   ///   Reports the engine version. The value is of type TX_STRING. <br />
	   ///   Deprecated, use TX_STATEPATH_ENGINEINFOVERSION for engine version
	   ///   1.3.0 and greater.
	   /// </summary>
	   TX_STATEPATH_ENGINEVERSION: PAnsiChar = 'engineVersion';
	 const
	   /// <summary>
	   ///   Holds the configuration status of the eye tracker. The value is of
	   ///   type TX_EYETRACKINGCONFIGURATIONSTATUS.
	   /// </summary>
	   TX_STATEPATH_CONFIGURATIONSTATUS: PAnsiChar = 'eyeTracking.configurationStatus';
	 const
	   /// <summary>
	   ///   Holds the virtual screen bounds in pixels. <br />* The value can be
	   ///   retrieved from the state bag as a TX_RECT structure with
	   ///   GetStateValueAsRectangle. <br />* If the screen bounds can not be
	   ///   determined screen bounds (0, 0, 0, 0) will be returned. <br />*
	   ///   Deprecated, use TX_STATEPATH_EYETRACKINGSCREENBOUNDS for engine
	   ///   version 1.3.0 and greater.
	   /// </summary>
	   TX_STATEPATH_SCREENBOUNDS: PAnsiChar = 'eyeTracking.screenBounds';
	 const
	   /// <summary>
	   ///   Holds the display size in millimeters as width and height. <br />*
	   ///   The value can be retrieved from the state bag as a TX_SIZE2 structure
	   ///   with GetStateValueAsSize2. <br />* If the display size can not be
	   ///   determined Width and Height (0, 0) will be returned. <br />*
	   ///   Deprecated, use TX_STATEPATH_EYETRACKINGDISPLAYSIZE for engine
	   ///   version 1.3.0 and greater.
	   /// </summary>
	   TX_STATEPATH_DISPLAYSIZE: PAnsiChar = 'eyeTracking.displaySize';

 (*********************************************************************************************************************)

 (*********************************************************************************************************************
 * Copyright 2013-2014 Tobii Technology AB. All rights reserved.
 * EyeXConstants.h
 *********************************************************************************************************************)

(*********************************************************************************************************************)

	const
	  /// <summary>
	  ///   Use this mask weight to indicate that a region of an interactor has no
	  ///   weight (not interactable).
	  /// </summary>
	  TX_MASKWEIGHT_NONE: Byte = 0;
	const
	  /// <summary>
	  ///   Use this mask weight to indicate that a region of an interactor has a
	  ///   default weight.
	  /// </summary>
	  TX_MASKWEIGHT_DEFAULT: Byte = 1;
	const
	  /// <summary>
	  ///   Use this mask weight to indicate that a region of an interactor has a
	  ///   high weight (more likely to be interacted with).
	  /// </summary>
	  TX_MASKWEIGHT_HIGH: Byte = 255;



(*********************************************************************************************************************)

(*********************************************************************************************************************
 * EyeXSharedLiterals.h
 *********************************************************************************************************************)
(*********************************************************************************************************************
 * Literals
 *********************************************************************************************************************)
    (**
    *    State literals
    *)
	 const TX_SHAREDLITERAL_STATEPATH: PAnsiChar = 'StatePath';

    (**
    *   Snapshot Literals
    *)
	 const TX_SHAREDLITERAL_WINDOWIDS: PAnsiChar = 'WindowIds';
	 const TX_SHAREDLITERAL_INTERACTORS: PAnsiChar = 'Interactors';
	 const TX_SHAREDLITERAL_METADATA: PAnsiChar = 'Metadata';
	 const TX_SHAREDLITERAL_SERIALNUMBER: PAnsiChar = 'SerialNumber';

	 const TX_SHAREDLITERAL_MESSAGETYPE: PAnsiChar = 'MessageType';
	 const TX_SHAREDLITERAL_WINDOWID: PAnsiChar = 'WindowId';
	 const TX_SHAREDLITERAL_COMMANDTYPE: PAnsiChar = 'CommandType';
	 const TX_SHAREDLITERAL_ISVISIBLE: PAnsiChar = 'IsVisible';

	(**
	*   Callback Response Literals
	*)
	 const TX_SHAREDLITERAL_NOTIFICATIONTYPE: PAnsiChar = 'NotificationType';

	(**
	*   Interactor Literals
	*)
	 const TX_SHAREDLITERAL_BEHAVIORTYPE: PAnsiChar = 'BehaviorType';
	 const TX_SHAREDLITERAL_BEHAVIORS: PAnsiChar = 'Behaviors';
	 const TX_SHAREDLITERAL_PARENTID: PAnsiChar = 'ParentId';
	 const TX_SHAREDLITERAL_INTERACTORID: PAnsiChar = 'InteractorId';
	 const TX_SHAREDLITERAL_ISENABLED: PAnsiChar = 'IsEnabled';
	 const TX_SHAREDLITERAL_ISDELETED: PAnsiChar = 'IsDeleted';

	 const TX_CONNECTIONTOKEN_GETVERSION: PAnsiChar = 'GET_VERSION';
	 const TX_CONNECTIONTOKEN_GETMINORVERSION: PAnsiChar = 'GET_MINORVERSION';
	 const TX_CONNECTIONTOKEN_CONNECT: PAnsiChar = 'CONNECT';
	 const TX_CONNECTIONTOKEN_CLIENTVERSION: PAnsiChar = 'CLIENT_VERSION';
	 const TX_CONNECTIONTOKEN_CLIENTMODE: PAnsiChar = 'CLIENT_MODE';
	 const TX_CONNECTIONTOKEN_CLIENTID: PAnsiChar = 'CLIENT_ID';
	 const TX_CONNECTIONTOKEN_LITERALLIST: PAnsiChar = 'LITERAL_LIST';

(*********************************************************************************************************************)

(*********************************************************************************************************************
 * Copyright 2013-2014 Tobii Technology AB. All rights reserved.
 * EyeXFrameworkTypes.h
 *********************************************************************************************************************)

type
  /// <summary>
  ///   Enumeration for all result codes returned by the API functions.
  /// </summary>
  TX_RESULT           = (
    /// <summary>
    ///   Unknown error, typically returned if something unexpected occurs in
    ///   the API. Is most likely a bug in the API.
    /// </summary>
    TX_RESULT_UNKNOWN = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   Everything went well.
    /// </summary>
    TX_RESULT_OK,
    /// <summary>
    ///   The EyeX client environment is not initalized. All API functions
    ///   except txInitializeEyeX requires the EyeX client environment to be
    ///   initialized prior to being called. <br />
    /// </summary>
    TX_RESULT_EYEXNOTINITIALIZED,
    /// <summary>
    ///   The EyeX client environment has already been initialized. This is
    ///   returned by txInitializeEyeX if called twice without being
    ///   uninitialized in between. <br />
    /// </summary>
    TX_RESULT_EYEXALREADYINITIALIZED,
    /// <summary>
    ///   The EyeX client environment is still in use. This is returned by
    ///   txUninitializeEyeX if at least one context is still being used.
    /// </summary>
    TX_RESULT_EYEXSTILLINUSE,
    /// <summary>
    ///   An invalid argument was passed to an API function. All arguments are
    ///   checked before an API function actually does something. There are
    ///   many reasons why an argument can be considered invalid. Check the log
    ///   for more details if this code is returned. <br />
    /// </summary>
    TX_RESULT_INVALIDARGUMENT,
    /// <summary>
    ///   The handle for an interaction object is not valid.
    /// </summary>
    TX_RESULT_INVALIDHANDLE,
    /// <summary>
    ///   Generic result code when something could not be found.
    /// </summary>
    TX_RESULT_NOTFOUND,
    /// <summary>
    ///   Some buffer; string, array, etc had an invalid size. Typically, API
    ///   functions that return this result code also provides the required
    ///   size. <br />
    /// </summary>
    TX_RESULT_INVALIDBUFFERSIZE,
    /// <summary>
    ///   An attempt has been made to create a property that does already
    ///   exist.
    /// </summary>
    TX_RESULT_DUPLICATEPROPERTY,
    /// <summary>
    ///   An attempt has been made to create bounds that already exists.
    /// </summary>
    TX_RESULT_DUPLICATEBOUNDS,
    /// <summary>
    ///   An attempt has been made to create a behavior that already exists.
    /// </summary>
    TX_RESULT_DUPLICATEBEHAVIOR,
    /// <summary>
    ///   An attempt has been made to create an interactor with the same id as
    ///   another in the same snapshot.
    /// </summary>
    TX_RESULT_DUPLICATEINTERACTOR,
    /// <summary>
    ///   An attempt has been made to register the same state observer twice.
    /// </summary>
    TX_RESULT_DUPLICATESTATEOBSERVER,
    /// <summary>
    ///   An attempt has been made to create more than one mask on an
    ///   interactor.
    /// </summary>
    TX_RESULT_DUPLICATEMASK,
    /// <summary>
    ///   A type specific operation has been made on a property of a different
    ///   type. For example a property containing a TX_INTEGER has been
    ///   requested for its value as a TX_STRING.
    /// </summary>
    TX_RESULT_INVALIDPROPERTYTYPE,
    /// <summary>
    ///   The specified property name is invalid.
    /// </summary>
    TX_RESULT_INVALIDPROPERTYNAME,
    /// <summary>
    ///   An attempt has been made to remove a property that is not removable.
    ///   Typically such properties are the ones backing up data that is
    ///   required on different interaction objects.
    /// </summary>
    TX_RESULT_PROPERTYNOTREMOVABLE,
    /// <summary>
    ///   An attempt was made to perform an operation that requires a valid
    ///   connection to the client.
    /// </summary>
    TX_RESULT_NOTCONNECTED,
    /// <summary>
    ///   A handle for a different type of interaction object than expected was
    ///   provided.
    /// </summary>
    TX_RESULT_INVALIDOBJECTCAST,
    /// <summary>
    ///   An attempt was made to perform an operation on a thread that is not
    ///   allowed to perform such an operation. For example a context can not
    ///   be deleted on a callback from the API. <br />
    /// </summary>
    TX_RESULT_INVALIDTHREAD,
    /// <summary>
    ///   An attempt was made to perform an operation that does not apply to
    ///   the current bounds type.
    /// </summary>
    TX_RESULT_INVALIDBOUNDSTYPE,
    /// <summary>
    ///   An attempt was made to perform an operation that does not apply to
    ///   the current behavior type.
    /// </summary>
    TX_RESULT_INVALIDBEHAVIORTYPE,
    /// <summary>
    ///   A leakage of an interaction object has been detected. May be returned
    ///   by a successful txReleaseContext call where some object were not
    ///   released properly.
    /// </summary>
    TX_RESULT_OBJECTLEAKAGE,
    /// <summary>
    ///   An attempt to retrieve tracked object has been made without tracking
    ///   of objects being enabled.
    /// </summary>
    TX_RESULT_OBJECTTRACKINGNOTENABLED,
    /// <summary>
    ///   The snapshot committed to the client contained some invalid data.
    /// </summary>
    TX_RESULT_INVALIDSNAPSHOT,
    /// <summary>
    ///   The submitted command was malformed or not recognized by the client.
    /// </summary>
    TX_RESULT_INVALIDCOMMAND,
    /// <summary>
    ///   An attempt has been made to perform an operation that is not
    ///   supported during shutdown.
    /// </summary>
    TX_RESULT_CANCELLED,
    /// <summary>
    ///   The scheduling mode is invalid.
    /// </summary>
    TX_RESULT_INVALIDSCHEDULINGMODE,
    /// <summary>
    ///   The supplied mask is too large, width*height must be less than 65536
    /// </summary>
    TX_RESULT_MASKTOOLARGE,
    /// <summary>
    ///   The submitted command can not be executed in the current state of the
    ///   eye tracker
    /// </summary>
    TX_RESULT_INVALIDEYETRACKERSTATE
  );
  TTxResult = TX_RESULT;
  PTxResult = ^TTxResult;


(*********************************************************************************************************************
 * Interaction Object Types
 *********************************************************************************************************************)

 /// <summary>
  ///   Enumeration for all the types of interaction objects that can be
  ///   exposed through the API.
  /// </summary>
  TX_INTERACTIONOBJECTTYPE                    = (
    /// <summary>
    ///   The object is a snapshot.
    /// </summary>
    TX_INTERACTIONOBJECTTYPE_SNAPSHOT         = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   The object is an interactor.
    /// </summary>
    TX_INTERACTIONOBJECTTYPE_INTERACTOR,
    /// <summary>
    ///   The object is a query.
    /// </summary>
    TX_INTERACTIONOBJECTTYPE_QUERY,
    /// <summary>
    ///   The object is an event.
    /// </summary>
    TX_INTERACTIONOBJECTTYPE_EVENT,
    /// <summary>
    ///   The object is a behavior.
    /// </summary>
    TX_INTERACTIONOBJECTTYPE_BEHAVIOR,
    /// <summary>
    ///   The object is a bounds structure.
    /// </summary>
    TX_INTERACTIONOBJECTTYPE_BOUNDS,
    /// <summary>
    ///   The object is a property bag. <br />
    /// </summary>
    TX_INTERACTIONOBJECTTYPE_PROPERTYBAG,
    /// <summary>
    ///   The object is a command.
    /// </summary>
    TX_INTERACTIONOBJECTTYPE_COMMAND,
    /// <summary>
    ///   The object is a state bag.
    /// </summary>
    TX_INTERACTIONOBJECTTYPE_STATEBAG,
    /// <summary>
    ///   The object is a notification.
    /// </summary>
    TX_INTERACTIONOBJECTTYPE_NOTIFICATION,
    /// <summary>
    ///   The object is a mask.
    /// </summary>
    TX_INTERACTIONOBJECTTYPE_MASK,
    /// <summary>
    ///   The object is an asyncdata structure.
    /// </summary>
    TX_INTERACTIONOBJECTTYPE_ASYNCDATA,
    { for internal use only }
    /// <summary>
    ///   The object is an internal message.
    /// </summary>
    TX_INTERACTIONOBJECTTYPE_INTERNAL_MESSAGE = TX_INTERNAL_ENUM_STARTVALUE,
    /// <summary>
    ///   The object is an internal messageheader.
    /// </summary>
    TX_INTERACTIONOBJECTTYPE_INTERNAL_MESSAGEHEADER
  );

  /// <summary>
  ///   <para>
  ///     Enumeration for all message types.
  ///   </para>
  ///   <para>
  ///     The messages type is metadata contained by all packets sent between
  ///     the client and server.
  ///   </para>
  ///   <para>
  ///     Some messages should be handled by the application to do proper
  ///     interaction, others are internal and should be ignored. <br />
  ///   </para>
  /// </summary>
  TX_MESSAGETYPE                = (
    /// <summary>
    ///   Message contains a query.
    /// </summary>
    TX_MESSAGETYPE_QUERY        = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   Message contains an event.
    /// </summary>
    TX_MESSAGETYPE_EVENT,
    /// <summary>
    ///   Message contains a notification. This is an internal message type.
    /// </summary>
    TX_MESSAGETYPE_NOTIFICATION = TX_INTERNAL_ENUM_STARTVALUE,
    /// <summary>
    ///   Message contains a request. This is an internal message type.
    /// </summary>
    TX_MESSAGETYPE_REQUEST,
    /// <summary>
    ///   Message contains a response. This is an internal message type.
    /// </summary>
    TX_MESSAGETYPE_RESPONSE,
    /// <summary>
    ///   Base value for custom message defined by other protocols.
    /// </summary>
    TX_MESSAGETYPE_CUSTOM
  );


  /// <summary>
  ///   <para>
  ///     Enumeration for all notification types.
  ///   </para>
  ///   <para>
  ///     The notification type is metadata contained by all notifications to
  ///     specify what kind of notification it is. <br />
  ///   </para>
  /// </summary>
  TX_NOTIFICATIONTYPE                = (
    /// <summary>
    ///   Notifies that some states have changed.
    /// </summary>
    TX_NOTIFICATIONTYPE_STATECHANGED = TX_ENUM_STARTVALUE,
    TX_NOTIFICATIONTYPE_DIAGNOSTICSDATA
  );

  /// <summary>
  ///   <para>
  ///     Enumeration for all behavior types. <br />
  ///   </para>
  ///   <para>
  ///     The behavior type is metadata contained by all behaviors to specify
  ///     what kind of behavior it is. <br />
  ///   </para>
  /// </summary>
  TX_BEHAVIORTYPE                        = (
    /// <summary>
    ///   Behavior used on interactors to receive gaze point data.
    /// </summary>
    TX_BEHAVIORTYPE_GAZEPOINTDATA        = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   Behavior used on interactors to receive eye position data.
    /// </summary>
    TX_BEHAVIORTYPE_EYEPOSITIONDATA,
    /// <summary>
    ///   Behavior used on interactors to perform gaze-aware interaction.
    /// </summary>
    TX_BEHAVIORTYPE_GAZEAWARE,
    /// <summary>
    ///   Behavior used on interactors to perform activation interaction. <br />
    /// </summary>
    TX_BEHAVIORTYPE_ACTIVATABLE,
    /// <summary>
    ///   Behavior used on interactors to perform panning interaction.
    /// </summary>
    TX_BEHAVIORTYPE_PANNABLE,
    /// <summary>
    ///   Behavior used on interactors to receive fixation data.
    /// </summary>
    TX_BEHAVIORTYPE_FIXATIONDATA,
    { For Internal use }
    TX_INTERNAL_BEHAVIORTYPE_RAWGAZEDATA = TX_INTERNAL_ENUM_STARTVALUE,
    TX_INTERNAL_BEHAVIORTYPE_ZOOMABLE,
    TX_BEHAVIORTYPE_GAZEDATADIAGNOSTICS
  );
  TTxBehaviortype = TX_BEHAVIORTYPE;
  PTxBehaviortype = ^TTxBehaviortype;

(*********************************************************************************************************************)

(**
  Enumeration for all bounds types.

  @field TX_BOUNDSTYPE_NONE:
    No bounds.

  @field TX_BOUNDSTYPE_RECTANGULAR:
    Rectangular bounds.
 *)
  TX_BOUNDSTYPE        = (
    TX_BOUNDSTYPE_NONE = TX_ENUM_STARTVALUE,
    TX_BOUNDSTYPE_RECTANGULAR
  );

  /// <summary>
  ///   <para>
  ///     Enumeration for all activation event types. <br />
  ///   </para>
  ///   <para>
  ///     Activatable event type are metadata contained by all behaviors of
  ///     type TX_BEHAVIORTYPE_ACTIVATABLE sent from the client. This event
  ///     type specifies what kind of activation event actually happened. <br />
  ///   </para>
  /// </summary>
  TX_ACTIVATABLEEVENTTYPE             = (
    /// <summary>
    ///   The interactor has been activated.
    /// </summary>
    TX_ACTIVATABLEEVENTTYPE_ACTIVATED = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   The activation focus and/or tentative activation focus has changed. <br />
    /// </summary>
    TX_ACTIVATABLEEVENTTYPE_ACTIVATIONFOCUSCHANGED
  );

  /// <summary>
  ///   Enumeration for all action data types.
  /// </summary>
  TX_FAILEDACTIONTYPE                   = (
    /// <summary>
    ///   An activation action did not hit any valid interactor <br />
    /// </summary>
    TX_FAILEDACTIONTYPE_ACTIVATIONNOHIT = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   A pan action did not hit any valid interactor <br />
    /// </summary>
    TX_FAILEDACTIONTYPE_PANNOHIT,
    /// <summary>
    ///   A zoom action did not hit any valid interactor
    /// </summary>
    TX_FAILEDACTIONTYPE_ZOOMNOHIT,
    /// <summary>
    ///   An activation action occured when there was no tracking <br />
    /// </summary>
    TX_FAILEDACTIONTYPE_ACTIVATIONDURINGNOTRACKING,
    /// <summary>
    ///   A pan action occured when there was no tracking <br />
    /// </summary>
    TX_FAILEDACTIONTYPE_PANDURINGNOTRACKING,
    /// <summary>
    ///   A zoom action occured when there was no tracking <br />
    /// </summary>
    TX_FAILEDACTIONTYPE_ZOOMDURINGNOTRACKING,
    /// <summary>
    ///   <para>
    ///     For internal use only.
    ///   </para>
    ///   <para>
    ///     An activation occured near multiple small interactors and was
    ///     therefore undecided <br />
    ///   </para>
    /// </summary>
    TX_FAILEDACTIONTYPE_ACTIVATIONSMALLITEMS
  );

  /// <summary>
  ///   <para>
  ///     Enumeration for all fixation data event types. <br />Fixation event
  ///     type is metadata contained by all behaviors of type
  ///   </para>
  ///   <para>
  ///     TX_BEHAVIORTYPE_FIXATION sent from the client. This event type
  ///     specifies what kind of fixation event actually happened.
  ///   </para>
  /// </summary>
  TX_FIXATIONDATAEVENTTYPE         = (
    /// <summary>
    ///   The fixation has begun. The gaze point data provided is a combination
    ///   of the gaze points used to detect the fixation. The timestamp will
    ///   reflect when the fixation actually began. <br />
    /// </summary>
    TX_FIXATIONDATAEVENTTYPE_BEGIN = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   The fixation has ended. The last valid gaze point is provided. The
    ///   timestamp will reflect when the fixation actually ended. <br />
    /// </summary>
    TX_FIXATIONDATAEVENTTYPE_END,
    /// <summary>
    ///   The fixation is still occurring. A new, filtered gaze point within
    ///   the fixation is provided.
    /// </summary>
    TX_FIXATIONDATAEVENTTYPE_DATA
  );
//  TTxFixationdataeventtype = TX_FIXATIONDATAEVENTTYPE;
//  PTxFixationdataeventtype = ^TTxFixationdataeventtype;

  /// <summary>
  ///   Enumeration for all gaze point data modes. <br />The gaze point data
  ///   mode is metadata contained by all behaviors of type
  ///   TX_BEHAVIORTYPE_GAZEPOINTDATA. <br />
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     When put on an interactor it specifies what kind of filter to use
  ///     by the engine when calculating the gaze points. <br />
  ///   </para>
  ///   <para>
  ///     When put on an event it specifies what kind of filter that was used
  ///     by the engine.
  ///   </para>
  /// </remarks>
  TX_GAZEPOINTDATAMODE              = (
    /// <summary>
    ///   No filter will be applied to the gaze points. (note though that
    ///   invalid gaze points are discarded) <br />
    /// </summary>
    TX_GAZEPOINTDATAMODE_UNFILTERED = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   A light filter was/will be applied to the gaze point data. It is not
    ///   a simple smoothing filter, it aims to be <br />smooth but responsive.
    ///   This should be you default choice for gaze point data. <br />
    /// </summary>
    TX_GAZEPOINTDATAMODE_LIGHTLYFILTERED
  );
//  TTxGazepointdatamode = TX_GAZEPOINTDATAMODE;
//  PTxGazepointdatamode = ^TTxGazepointdatamode;


  /// <summary>
  ///   Enumeration for all gaze aware modes. The gaze aware mode is metadata
  ///   contained by all behaviors of type TX_BEHAVIORTYPE_GAZEAWARE. <br /><br />
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     When put on an interactor it specifies how the user must gaze on
  ///     the interactor to make it gaze aware. <br />
  ///   </para>
  ///   <para>
  ///     When put on an event it specifies what kind of mode that was used
  ///     by the engine.
  ///   </para>
  /// </remarks>
  TX_GAZEAWAREMODE          = (
    /// <summary>
    ///   The interactor will get a gaze aware event when the engine considers
    ///   the user to have looked at it for a specified amount of time. When
    ///   using this mode TX_GAZEAWAREPARAMS needs have the field DelayTime
    ///   set. <br />
    /// </summary>
    TX_GAZEAWAREMODE_NORMAL = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   The interactor will get a gaze aware event when the engine considers
    ///   the user to intentionally look at it. <br />
    /// </summary>
    TX_GAZEAWAREMODE_DELAYED
  );
//  TTxGazeawaremode = TX_GAZEAWAREMODE;
//  PTxGazeawaremode = ^TTxGazeawaremode;

  /// <summary>
  ///   <para>
  ///     Enumeration for all fixation data modes. <br />
  ///   </para>
  ///   <para>
  ///     The fixation data mode is metadata contained by all behaviors of
  ///     type TX_BEHAVIORTYPE_FIXATION.
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   When put on an interactor it specifies what kind of filter to use by
  ///   the engine when finding fixations. <br /><br />When put on an event it
  ///   specifies what kind of filter that was used by the engine.
  /// </remarks>
  TX_FIXATIONDATAMODE             = (
    /// <summary>
    ///   Very sensitive fixation filter, will result in many fixations,
    ///   sometimes very close and in quick succession. <br />
    /// </summary>
    TX_FIXATIONDATAMODE_SENSITIVE = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   Fairly sensitive to enter fixation but can be slow to exit, as it
    ///   tries merge fixations close to each other. <br />Will result in
    ///   fairly stable fixations but fixation end events may be coming rather
    ///   late in certain circumstances. <br />
    /// </summary>
    TX_FIXATIONDATAMODE_SLOW
  );
//  TTxFixationdatamode = TX_FIXATIONDATAMODE;
//  PTxFixationdatamode = ^TTxFixationdatamode;

  /// <summary>
  ///   Enumeration for all eye tracking device statuses. <br />
  /// </summary>
  TX_EYETRACKINGDEVICESTATUS                = (
    /// <summary>
    ///   The eye tracking device is initializing. <br />
    /// </summary>
    TX_EYETRACKINGDEVICESTATUS_INITIALIZING = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   There is no eye tracking device available. <br />
    /// </summary>
    TX_EYETRACKINGDEVICESTATUS_NOTAVAILABLE,
    /// <summary>
    ///   The eye tracking device has an invalid configuration.
    /// </summary>
    TX_EYETRACKINGDEVICESTATUS_INVALIDCONFIGURATION,
    /// <summary>
    ///   The eye tracking device is not connected. <br />
    /// </summary>
    TX_EYETRACKINGDEVICESTATUS_DEVICENOTCONNECTED,
    /// <summary>
    ///   The eye tracking device is currently tracking.
    /// </summary>
    TX_EYETRACKINGDEVICESTATUS_TRACKING,
    /// <summary>
    ///   The eye tracking device is paused.
    /// </summary>
    TX_EYETRACKINGDEVICESTATUS_TRACKINGPAUSED,
    /// <summary>
    ///   The eye tracking device is being configured. <br />
    /// </summary>
    TX_EYETRACKINGDEVICESTATUS_CONFIGURING,
    /// <summary>
    ///   Unknown error.
    /// </summary>
    TX_EYETRACKINGDEVICESTATUS_UNKNOWNERROR,
    /// <summary>
    ///   The eye tracking device is connected to USB port but EyeX Engine can
    ///   not connect to it. <br />
    /// </summary>
    TX_EYETRACKINGDEVICESTATUS_CONNECTIONERROR,
    /// <summary>
    ///   The eye tracking device is functioning as intended, but no gaze data
    ///   is sent. <br />
    /// </summary>
    TX_EYETRACKINGDEVICESTATUS_TRACKINGUNAVAILABLE
  );

(**
 *)
  /// <summary>
  ///   Enumeration for all command types. For internal use only.
  /// </summary>
  TX_COMMANDTYPE                 = (
    TX_COMMANDTYPE_EXECUTEACTION = TX_ENUM_STARTVALUE,
    TX_COMMANDTYPE_SETSTATE,
    TX_COMMANDTYPE_GETSTATE,
    TX_COMMANDTYPE_REGISTERSTATEOBSERVER,
    TX_COMMANDTYPE_UNREGISTERSTATEOBSERVER,
    TX_COMMANDTYPE_COMMITSNAPSHOT,
    TX_COMMANDTYPE_ENABLEBUILTINKEYS,
    TX_COMMANDTYPE_DISABLEBUILTINKEYS,
    TX_COMMANDTYPE_CLIENTCONNECTION,
    TX_COMMANDTYPE_LAUNCHEYETRACKINGCONTROLPANEL,  {* Deprecated }
    TX_COMMANDTYPE_REGISTERQUERYHANDLER,
    TX_COMMANDTYPE_UNREGISTERQUERYHANDLER,
    TX_COMMANDTYPE_DIAGNOSTICSREQUEST,
    TX_COMMANDTYPE_LAUNCHCONFIGURATIONTOOL,
    TX_COMMANDTYPE_SETCURRENTPROFILE,
    TX_COMMANDTYPE_DELETEPROFILE,
    TX_COMMANDTYPE_CLIENTPROCESSIDLIST
  );

  /// <summary>
  ///   <para>
  ///     Enumeration for all action types.
  ///   </para>
  ///   <para>
  ///     An action is a way to interact with the EyeX Engine in addition or
  ///     instead of the default keybindings that normally executes gaze
  ///     actions. <br />
  ///   </para>
  /// </summary>
  TX_ACTIONTYPE            = (
    /// <summary>
    ///   Activates an interactor. This corresponds to a click on the
    ///   activation button.
    /// </summary>
    TX_ACTIONTYPE_ACTIVATE = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   Turns on activation mode. This corresponds to pressing the activation
    ///   button.
    /// </summary>
    TX_ACTIONTYPE_ACTIVATIONMODEON,
    /// <summary>
    ///   Turns off activation mode. This corresponds to releasing the
    ///   activation button.
    /// </summary>
    TX_ACTIONTYPE_ACTIVATIONMODEOFF,
    /// <summary>
    ///   Begins a panning. This corresponds to pressing the panning button.
    /// </summary>
    TX_ACTIONTYPE_PANNINGBEGIN,
    /// <summary>
    ///   Ends a panning. This corresponds to releasing the panning button.
    /// </summary>
    TX_ACTIONTYPE_PANNINGEND,
    /// <summary>
    ///   Performs a panning step action. This corresponds to a click on the
    ///   panning button.
    /// </summary>
    TX_ACTIONTYPE_PANNINGSTEP,
    /// <summary>
    ///   Not yet supported.
    /// </summary>
    TX_ACTIONTYPE_ZOOMIN,
    /// <summary>
    ///   Not yet supported.
    /// </summary>
    TX_ACTIONTYPE_ZOOMOUT,
    /// <summary>
    ///   Not yet supported.
    /// </summary>
    TX_ACTIONTYPE_PANNINGTOGGLEHANDSFREE
  );


  /// <summary>
  ///   <para>
  ///     Enumeration for all pannable event types.
  ///   </para>
  ///   <para>
  ///     Pannable event type are metadata contained by all behaviors of type
  ///     TX_BEHAVIORTYPE_PANNABLE sent from the client. This event type
  ///     specifies what kind of pannable event actually happened. <br />
  ///   </para>
  /// </summary>
  TX_PANNABLEEVENTTYPE       = (
    /// <summary>
    ///   The interactor has been panned.
    /// </summary>
    TX_PANNABLEEVENTTYPE_PAN = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   The interactor has been stepped.
    /// </summary>
    TX_PANNABLEEVENTTYPE_STEP,
    /// <summary>
    ///   Not yet supported.
    /// </summary>
    TX_PANNABLEEVENTTYPE_HANDSFREE
  );


  /// <summary>
  ///   <para>
  ///     Enumeration flags for all pannable directions.
  ///   </para>
  ///   <para>
  ///     Governs available directions to pan for a pannable interactor. <br />
  ///     The directions are bitwise combinable. <br />
  ///   </para>
  /// </summary>
  TX_PANDIRECTION         = (
    /// <summary>
    ///   No pandirection available.
    /// </summary>
    TX_PANDIRECTION_NONE  = TX_FLAGS_NONE_VALUE,
    /// <summary>
    ///   Panning to the left available.
    /// </summary>
    TX_PANDIRECTION_LEFT  = 1,
    /// <summary>
    ///   Panning to the right available.
    /// </summary>
    TX_PANDIRECTION_RIGHT = 1  shl 1,
    /// <summary>
    ///   Panning up available.
    /// </summary>
    TX_PANDIRECTION_UP    = 1  shl 2,
    /// <summary>
    ///   Panning down available.
    /// </summary>
    TX_PANDIRECTION_DOWN  = 1  shl 3,
    /// <summary>
    ///   All pan directions available.
    /// </summary>
    TX_PANDIRECTION_ALL   = (1  shl 4) - 1
  );

  /// <summary>
  ///   Enumeration for all panning profiles.
  /// </summary>
  TX_PANNINGPROFILE        = (
    /// <summary>
    ///   No panning profile.
    /// </summary>
    TX_PANNINGPROFILE_NONE = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   Panning profile for reading, currently same as
    ///   TX_PANNINGPROFILE_VERTICAL. Will be available in subsequent versions.
    /// </summary>
    TX_PANNINGPROFILE_READING,
    /// <summary>
    ///   Left and right only panning profile.
    /// </summary>
    TX_PANNINGPROFILE_HORIZONTAL,
    /// <summary>
    ///   Up and down only panning profile.
    /// </summary>
    TX_PANNINGPROFILE_VERTICAL,
    /// <summary>
    ///   Up, down, left and right, with emphasis on vertical panning.
    /// </summary>
    TX_PANNINGPROFILE_VERTICALFIRSTTHENHORIZONTAL,
    /// <summary>
    ///   Panning in any direction.
    /// </summary>
    TX_PANNINGPROFILE_RADIAL,
    /// <summary>
    ///   Up, down, left and right, with emphasis on horizontal panning.
    /// </summary>
    TX_PANNINGPROFILE_HORIZONTALFIRSTTHENVERTICAL
  );

  /// <summary>
  ///   Enumeration for conveying gaze tracking status.
  /// </summary>
  TX_GAZETRACKING               = (
    /// <summary>
    ///   Gaze is currently being tracked.
    /// </summary>
    TX_GAZETRACKING_GAZETRACKED = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   Gaze is currently not being tracked. This state can be set when for
    ///   example the user is gazing outside of the tracked display, no user is
    ///   present in front of the eye tracker, or that no connection is
    ///   established with the eye tracker etc. <br />
    /// </summary>
    TX_GAZETRACKING_GAZENOTTRACKED
  );

  /// <summary>
  ///   Enumeration for conveying presence status.
  /// </summary>
  TX_USERPRESENCE           = (
    /// <summary>
    ///   A user is present in front of the eye tracker.
    /// </summary>
    TX_USERPRESENCE_PRESENT = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   A user is not present in front of the eye tracker.
    /// </summary>
    TX_USERPRESENCE_NOTPRESENT,
    /// <summary>
    ///   It is unknown whether or not a user is present in front of the eye
    ///   tracker. This value will be returned if there is no observer
    ///   registered for TX_STATEPATH_USERPRESENCE. <br />
    /// </summary>
    TX_USERPRESENCE_UNKNOWN
  );

  /// <summary>
  ///   Enumeration for all the types of requests that can be exposed through
  ///   the API.
  /// </summary>
  TX_REQUESTTYPE           = (
    /// <summary>
    ///   The request handles a command.
    /// </summary>
    TX_REQUESTTYPE_COMMAND = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   Base value for custom requests defined by other protocols.
    /// </summary>
    TX_REQUESTTYPE_CUSTOM  = TX_INTERNAL_ENUM_STARTVALUE
  );

  /// <summary>
  ///   Enumeration for mask types. <br />
  /// </summary>
  TX_MASKTYPE           = (
    /// <summary>
    ///   Default mask type.
    /// </summary>
    TX_MASKTYPE_DEFAULT = TX_ENUM_STARTVALUE
  );

  /// <summary>
  ///   Flags for describing engine interaction modes. These influence what
  ///   behaviors are being treated and what interaction behavior events are
  ///   being generated.
  /// </summary>
  TX_INTERACTIONMODES                  = (
    /// <summary>
    ///   Engine is not in any specific interacion mode, gaze aware behaviors
    ///   and data stream behaviors are being treated only. <br />
    /// </summary>
    TX_INTERACTIONMODES_NONE           = TX_FLAGS_NONE_VALUE,
    /// <summary>
    ///   Engine is in activation mode, meaning activatable interactors are
    ///   prioritized and activation events are being generated. <br />
    /// </summary>
    TX_INTERACTIONMODES_ACTIVATIONMODE = 1,
    /// <summary>
    ///   Engine is in panning mode, meaning pannable interactors are being
    ///   prioritzed, and appropriate events being generated. <br />
    /// </summary>
    TX_INTERACTIONMODES_PANNINGMODE    = 1  shl 2
  );



  TX_CLIENTMODE         = (
    TX_CLIENTMODE_AGENT = TX_ENUM_STARTVALUE,
    TX_CLIENTMODE_DIAGNOSTICS
  );


  /// <summary>
  ///   Enumeration for configuration tools. <br />
  /// </summary>
  TX_CONFIGURATIONTOOL                = (
    /// <summary>
    ///   EyeX Settings. Always available.
    /// </summary>
    TX_CONFIGURATIONTOOL_EYEXSETTINGS = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   <para>
    ///     Re-Calibrate the current profile.
    ///   </para>
    ///   <para>
    ///     Available when the following is fulfilled: <br />- Eye Tracking
    ///     Device Status is "Tracking", "TrackingPaused" or
    ///     "InvalidConfiguration". <br />- Eye Tracking Configuration Status
    ///     is "Valid" or "InvalidCalibration". <br />- State
    ///     TX_STATEPATH_EYETRACKINGCURRENTPROFILENAME has a value other than
    ///     empty string. <br /><br />When the Recalibrate tool is active the
    ///     Eye Tracking Device Status will be "Configuring", i.e. tracking
    ///     is off. <br />
    ///   </para>
    /// </summary>
    TX_CONFIGURATIONTOOL_RECALIBRATE,
    /// <summary>
    ///   <para>
    ///     Create and calibrate a guest profile and set it as active
    ///     profile.
    ///   </para>
    ///   <para>
    ///     Available when the following is fulfilled: <br />- Eye Tracking
    ///     Device Status is "Tracking", "TrackingPaused" or
    ///     "InvalidConfiguration". <br />- Eye Tracking Configuration Status
    ///     is "Valid" or "InvalidCalibration". <br /><br />When the Guest
    ///     Calibration tool is active the Eye Tracking Device Status will be
    ///     "Configuring", i.e. tracking is off. <br />
    ///   </para>
    /// </summary>
    TX_CONFIGURATIONTOOL_GUESTCALIBRATION,
    /// <summary>
    ///   <para>
    ///     Create and calibrate a new profile and set it as active profile.
    ///   </para>
    ///   <para>
    ///     Available when the following is fulfilled: <br />- Eye Tracking
    ///     Device Status is "Tracking", "TrackingPaused" or
    ///     "InvalidConfiguration". <br />- Eye Tracking Configuration Status
    ///     is "Valid" or "InvalidCalibration". <br /><br />When the Create
    ///     New Profile tool is active the Eye Tracking Device Status will be
    ///     "Configuring", i.e. tracking is off. <br />
    ///   </para>
    /// </summary>
    TX_CONFIGURATIONTOOL_CREATENEWPROFILE,
    /// <summary>
    ///   <para>
    ///     Test your eye tracking. <br />
    ///   </para>
    ///   <para>
    ///     Available when the following is fulfilled: <br />- Eye Tracking
    ///     Device Status is "Tracking". <br />
    ///   </para>
    /// </summary>
    TX_CONFIGURATIONTOOL_TESTEYETRACKING,
    /// <summary>
    ///   <para>
    ///     Diagnose your eye tracking. <br />
    ///   </para>
    ///   <para>
    ///     Always available.
    ///   </para>
    /// </summary>
    TX_CONFIGURATIONTOOL_DIAGNOSTICS,
    { for internal use only }
    TX_CONFIGURATIONTOOL_SETUPDISPLAY = TX_INTERNAL_ENUM_STARTVALUE,
    TX_CONFIGURATIONTOOL_EYEPOSITION,
    TX_CONFIGURATIONTOOL_EYECAPTURE,
    TX_CONFIGURATIONTOOL_FIRMWARE_UPGRADE,
    TX_CONFIGURATIONTOOL_CHECKFORUPDATES,
    TX_CONFIGURATIONTOOL_RETAILCALIBRATION
  );

  /// <summary>
  ///   Enumeration for configuration status. Gives information about the
  ///   configuration status of the eye tracker, for example if it needs to be
  ///   calibrated or if we need to setup display. Can be used as input to
  ///   determine when to enable launching of configuration tools, see
  ///   txLaunchConfigurationTool and TX_CONFIGURATIONTOOL. <br />
  /// </summary>
  TX_EYETRACKINGCONFIGURATIONSTATUS         = (
    /// <summary>
    ///   The configuration status of the eye tracker is valid.
    /// </summary>
    TX_EYETRACKINGCONFIGURATIONSTATUS_VALID = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   The monitor where the eye tracker is mounted need to be configured.
    /// </summary>
    TX_EYETRACKINGCONFIGURATIONSTATUS_INVALIDMONITORCONFIGURATION,
    /// <summary>
    ///   The eye tracker need to be calibrated. If no user profile exists (see
    ///   state TX_STATEPATH_EYETRACKINGCURRENTPROFILENAME) a new profile
    ///   should be created.
    /// </summary>
    TX_EYETRACKINGCONFIGURATIONSTATUS_INVALIDCALIBRATION,
    /// <summary>
    ///   The configuration is in an unknown error state.
    /// </summary>
    TX_EYETRACKINGCONFIGURATIONSTATUS_UNKNOWNERROR
  );

  /// <summary>
  ///   Enumeration for tracked eye of current profile.
  /// </summary>
  TX_TRACKEDEYES        = (
    /// <summary>
    ///   Track both eyes.
    /// </summary>
    TX_TRACKEDEYES_BOTH = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   Track left eye only.
    /// </summary>
    TX_TRACKEDEYES_ONLY_LEFT_EYE,
    /// <summary>
    ///   Track right eye only.
    /// </summary>
    TX_TRACKEDEYES_ONLY_RIGHT_EYE
  );


  /// <summary>
  ///   Enumeration for the different hands free modes. Used in
  ///   TX_PANNABLE_PARAMS for a specific behavior/interactor <br />
  /// </summary>
  TX_HANDSFREEPANNINGMODE           = (
    /// <summary>
    ///   Handsfree panning follows the current engine panning handsfree state,
    ///   which may be set by the user.
    /// </summary>
    TX_HANDSFREEPANNINGMODE_DEFAULT = 0,
    /// <summary>
    ///   Handsfree panning is always enabled, regardless of engine panning
    ///   handsfree state.
    /// </summary>
    TX_HANDSFREEPANNINGMODE_ALWAYSENABLED,
    /// <summary>
    ///   Handsfree panning is always disabled, regardless of engine panning
    ///   handsfree state.
    /// </summary>
    TX_HANDSFREEPANNINGMODE_ALWAYSDISABLED
  );

(*********************************************************************************************************************)

 (*********************************************************************************************************************
 * EyeXClientTypes.h
 *********************************************************************************************************************)
(*********************************************************************************************************************
 * Common types
 *********************************************************************************************************************)

  PTxUserparam = ^TTxUserparam;
  TX_USERPARAM = Pointer;
  TTxUserparam = Pointer;
  TX_HANDLE = pointer;// ^txInteractionObject;
  PTXHANDLE = ^TX_HANDLE;
  TX_CONSTHANDLE = pointer;//^txInteractionObject;
  TX_PROPERTYHANDLE = pointer;//^txProperty;
  TX_CONSTPROPERTYHANDLE = pointer;//^txProperty;
  TX_CONTEXTHANDLE = pointer;//^txContext;
  TX_CONSTCONTEXTHANDLE = pointer;//^txContext;
  PTxTicket = ^TTxTicket;
  TX_TICKET = Integer;
  TTxTicket = Integer;
  PTxBool = ^TTxBool;
  TX_BOOL = Integer;
  TTxBool = Integer;
  PTxByte = ^TTxByte;
  TX_BYTE = Byte;
  TTxByte = Byte;
  PTxSize = ^TTxSize;
  TX_SIZE = Integer;
  TTxSize = Integer;
  PTxInteger = ^TTxInteger;
  TX_INTEGER = Integer;
  TTxInteger = Integer;
  PTxReal = ^TTxReal;
  TX_REAL = Double;
  TTxReal = Double;
  PTxChar = ^TTxChar;
  TX_CHAR = AnsiChar;
  TTxChar = AnsiChar;
  TX_STRING = PAnsiChar;
  TX_CONSTSTRING = PAnsiChar;
  PTxRawptr = ^TTxRawptr;
  TX_RAWPTR = Pointer;
  TTxRawptr = Pointer;
  PTxThreadid = ^TTxThreadid;
  TX_THREADID = Integer;
  TTxThreadid = Integer;

(*********************************************************************************************************************)

//#include "EyeXInternalTypes.h"
(**

  @param functionName [in]:
	A TX_CONSTSTRING

  @param parameterName [in]:
	A TX_CONSTSTRING

  @param userParam [in]:
	Supplied when registering the callback, normally used to respond to the event outside of the callback.

  @return
	void
 *)
/// <summary>
///   Callback for an Invalid argument. <br />
/// </summary>
TX_INVALIDARGUMENTCALLBACK = procedure (
  functionName: TX_CONSTSTRING;
  parameterName: TX_CONSTSTRING;
  userParam: TX_USERPARAM
);     {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


(*********************************************************************************************************************)

  PTxPropertyflags = ^TTxPropertyflags;
  {$EXTERNALSYM TX_PROPERTYFLAGS}
  TX_PROPERTYFLAGS                = (
	TX_PROPERTYFLAG_NONE          = TX_FLAGS_NONE_VALUE,
    TX_PROPERTYFLAG_NONREMOVABLE  = 1  shl 0,
    TX_PROPERTYFLAG_MANUALCLONING = 1  shl 1
  );
  TTxPropertyflags = TX_PROPERTYFLAGS;


(*********************************************************************************************************************)
const
  TX_EMPTY_HANDLE         = nil;
  TX_INVALID_TICKET: TX_Ticket        = 0;

  TX_TRUE                             = 1;
  TX_FALSE                            = 0;

  TX_CLEANUPTIMEOUT_DEFAULT           = 500;
  TX_CLEANUPTIMEOUT_FORCEIMMEDIATE    =-1;

(*********************************************************************************************************************)

type

  /// <summary>
  ///   Enumeration for all client environment component override flags. <br />
  ///   When calling txInitializeEyeX these flags must be combined to specify
  ///   which components should be overridden. <br />
  /// </summary>
  TX_EYEXCOMPONENTOVERRIDEFLAGS                           = (
    /// <summary>
    ///   No client environment component should be overridden.
    /// </summary>
    TX_EYEXCOMPONENTOVERRIDEFLAG_NONE                     = TX_FLAGS_NONE_VALUE,
    /// <summary>
    ///   The logging model should be overridden. <br />The logging model can
    ///   be overridden by just specifying some of the standard log targets
    ///   (see TX_LOGTARGET) or by a custom user implemented log writer. <br />
    /// </summary>
    TX_EYEXCOMPONENTOVERRIDEFLAG_LOGGINGMODEL             = 1  shl 0,
    /// <summary>
    ///   The memory model should be overridden. For internal use only.
    /// </summary>
    TX_EYEXCOMPONENTOVERRIDEFLAG_INTERNAL_MEMORYMODEL     = 1  shl 1,
    /// <summary>
    ///   The threading model should be overridden. For internal use only.
    /// </summary>
    TX_EYEXCOMPONENTOVERRIDEFLAG_INTERNAL_THREADINGMODEL  = 1  shl 2,
    /// <summary>
    ///   The scheduling model should be overridden. For internal use only.
    /// </summary>
    TX_EYEXCOMPONENTOVERRIDEFLAG_INTERNAL_SCHEDULINGMODEL = 1  shl 3
  );
//  TTxEyexcomponentoverrideflags = TX_EYEXCOMPONENTOVERRIDEFLAGS;
//  PTxEyexcomponentoverrideflags = ^TTxEyexcomponentoverrideflags;

(*********************************************************************************************************************)

  /// <summary>
  ///   Enumeration for all connection states. <br />These values are used to
  ///   notify the application of the current connection state. <br />To
  ///   receive these notifications the client needs to subscribe using
  ///   txRegisterConnectionStateChangedHandler and then call
  ///   txEnableConnection. <br />
  /// </summary>
  TX_CONNECTIONSTATE             = (
    /// <summary>
    ///   The client is now connected to the server.
    /// </summary>
    TX_CONNECTIONSTATE_CONNECTED = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   The client is now disconnected from the server. Unless this is due to
    ///   TxDisableConnection being called the client will shortly attempt to
    ///   connect again.
    /// </summary>
    TX_CONNECTIONSTATE_DISCONNECTED,
    /// <summary>
    ///   The client is now trying to connect to the server. This is the first
    ///   state being sent to the application after txEnableConnection has been
    ///   called.
    /// </summary>
    TX_CONNECTIONSTATE_TRYINGTOCONNECT,
    /// <summary>
    ///   the server version is too low. The client is not connected and will
    ///   not try to reconnect.
    /// </summary>
    TX_CONNECTIONSTATE_SERVERVERSIONTOOLOW,
    /// <summary>
    ///   the server version is too high. The client is not connected and will
    ///   not try to reconnect.
    /// </summary>
    TX_CONNECTIONSTATE_SERVERVERSIONTOOHIGH
  );
//  TTxConnectionstate = TX_CONNECTIONSTATE;
//  PTxConnectionstate = ^TTxConnectionstate;

(*********************************************************************************************************************)

  /// <summary>
  ///   Enumeration for all log targets. <br />When overriding the logging
  ///   model these flags specify which log targets to use. The flags can be
  ///   combined.
  /// </summary>
  TX_LOGTARGET           = (
    /// <summary>
    ///   No logging should occur at all.
    /// </summary>
    TX_LOGTARGET_NONE    = TX_FLAGS_NONE_VALUE,
    /// <summary>
    ///   The log message should be written to the console.
    /// </summary>
    TX_LOGTARGET_CONSOLE = 1  shl 0,
    /// <summary>
    ///   The log messages should be traced. (output window i Visual Studio)
    /// </summary>
    TX_LOGTARGET_TRACE   = 1  shl 1,
    /// <summary>
    ///   The specified TX_LOGCALLBACK should be invoked for custom logging.
    /// </summary>
    TX_LOGTARGET_CUSTOM  = 1  shl 2
  );
//  TTxLogtarget = TX_LOGTARGET;
//  PTxLogtarget = ^TTxLogtarget;

(*********************************************************************************************************************)

  /// <summary>
  ///   Enumeration for all log levels. <br />The log levels are used to
  ///   indicate the severity of the message.
  /// </summary>
  TX_LOGLEVEL         = (
    /// <summary>
    ///   The message is just a debug print out typically used during
    ///   development.
    /// </summary>
    TX_LOGLEVEL_DEBUG = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   The message is plain info and does not indciate that something is
    ///   wrong.
    /// </summary>
    TX_LOGLEVEL_INFO,
    /// <summary>
    ///   The message is a warning that indicates that something is not the way
    ///   it should, not yet critical.
    /// </summary>
    TX_LOGLEVEL_WARNING,
    /// <summary>
    ///   The message indicates that there is some kind of error.
    /// </summary>
    TX_LOGLEVEL_ERROR
  );
//  TTxLoglevel = TX_LOGLEVEL;
//  PTxLoglevel = ^TTxLoglevel;

(*********************************************************************************************************************)

  /// <summary>
  ///   Enumeration for all schedulng modes. <br />When overriding the
  ///   scheduling model the mode specifies which of the available scheduling
  ///   modes to use.
  /// </summary>
  TX_SCHEDULINGMODE          = (
    /// <summary>
    ///   All jobs are performed immediately on the thread that calls them.
    /// </summary>
    TX_SCHEDULINGMODE_DIRECT = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   All jobs are performed when the txPerformScheduledJobs are called.
    /// </summary>
    TX_SCHEDULINGMODE_USERFRAME,
    /// <summary>
    ///   Whenever a job is to be performed a callback function is invoked
    ///   giving the client application full control.
    /// </summary>
    TX_SCHEDULINGMODE_CUSTOM
  );
//  TTxSchedulingmode = TX_SCHEDULINGMODE;
//  PTxSchedulingmode = ^TTxSchedulingmode;

(*********************************************************************************************************************)

  /// <summary>
  ///   Enumeration for all property value types. <br />
  /// </summary>
  TX_PROPERTYVALUETYPE         = (
    /// <summary>
    ///   The property does not have a value.
    /// </summary>
    TX_PROPERTYVALUETYPE_EMPTY = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   The property currently holds an interaction object.
    /// </summary>
    TX_PROPERTYVALUETYPE_OBJECT,
    /// <summary>
    ///   The property currently holds an integer.
    /// </summary>
    TX_PROPERTYVALUETYPE_INTEGER,
    /// <summary>
    ///   The property currently holds a real.
    /// </summary>
    TX_PROPERTYVALUETYPE_REAL,
    /// <summary>
    ///   The property currently holds a string.
    /// </summary>
    TX_PROPERTYVALUETYPE_STRING,
    /// <summary>
    ///   The property currently holds a blob.
    /// </summary>
    TX_PROPERTYVALUETYPE_BLOB
  );
//  TTxPropertyvaluetype = TX_PROPERTYVALUETYPE;
//  PTxPropertyvaluetype = ^TTxPropertyvaluetype;

(*********************************************************************************************************************)


  /// <summary>
  ///   Enumeration for the all Property Bag types.
  /// </summary>
  TX_PROPERTYBAGTYPE          = (
    /// <summary>
    ///   The property is a normal object with named properties.
    /// </summary>
    TX_PROPERTYBAGTYPE_OBJECT = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   The property bag is an array with sequentially named properties
    ///   appearing in the order they where inserted.
    /// </summary>
    TX_PROPERTYBAGTYPE_ARRAY
  );
//  TTxPropertybagtype = TX_PROPERTYBAGTYPE;
//  PTxPropertybagtype = ^TTxPropertybagtype;

(*********************************************************************************************************************)

  /// <summary>
  ///   Enumeration for the availability status of the EyeX Engine. <br />
  /// </summary>
  TX_EYEXAVAILABILITY                = (
    /// <summary>
    ///   EyeX Engine is not installed on the system or otherwise not
    ///   available.
    /// </summary>
    TX_EYEXAVAILABILITY_NOTAVAILABLE = TX_ENUM_STARTVALUE,
    /// <summary>
    ///   EyeX Engine is not running.
    /// </summary>
    TX_EYEXAVAILABILITY_NOTRUNNING,
    /// <summary>
    ///   EyeX Engine is running.
    /// </summary>
    TX_EYEXAVAILABILITY_RUNNING
  );
//  TTxEyexavailability = TX_EYEXAVAILABILITY;
//  PTxEyexavailability = ^TTxEyexavailability;


(*********************************************************************************************************************
 * Callbacks
 *********************************************************************************************************************)

(**
  Callback for when the connection state is changed.
    See txRegisterConnectionStateChangedHandler

  @param state [in]:
    Specifies the current state of the connection.

  @param userParam [in]:
    The user parameter provided to the txRegisterConnectionStateChangedHandler function.

  @return
    void
 *)
TX_CONNECTIONSTATECHANGEDCALLBACK = procedure (
  state: TX_CONNECTIONSTATE;
  userParam: TX_USERPARAM
); {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


(**
  Callback for an asynchronous operations.

  @param hAsyncData [in]:
    A TX_CONSTHANDLE to the async data.

  @param userParam [in]:
    The user parameter provided to the asynchronous operation.

  @return
    void
 *)
TX_ASYNCDATACALLBACK = procedure (
  hAsyncData: TX_CONSTHANDLE;
  userParam: TX_USERPARAM
); {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


(**
  Function run by a thread.
    See txInitializeEyeX, TX_THREADINGMODEL

  @param threadWorkerParam [in]:
    The user parameter provided to the CreateThreadCallback.

  @return
    void
 *)
TX_THREADWORKERFUNCTION = procedure (
  threadWorkerParam: TX_USERPARAM
); {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


(*********************************************************************************************************************)

(**
  Callback used to create a thread.
    See txInitializeEyeX, TX_THREADINGMODEL

  @param worker [in]:
   Worker function that will be run by the thread.

  @param threadWorkerParam [in]:
    A user parameter passed to worker function.

  @param userParam [in]:
    The user parameter provided by the TX_THREADINGMODEL structure.

  @return
    TX_THREADID, the id of the created thread.
 *)
TX_CREATETHREADCALLBACK = function (
  worker: TX_THREADWORKERFUNCTION;
  threadWorkerParam: TX_USERPARAM;
  userParam: TX_USERPARAM
): TX_THREADID; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


(*********************************************************************************************************************)

(**
  Callback used to get the current thread id.
    See txInitializeEyeX, TX_THREADINGMODEL

  @param userParam [in]:
    The user parameter provided by the TX_THREADINGMODEL structure.

  @return
    TX_THREADID, the id of the current thread
 *)
TX_GETCURRENTTHREADIDCALLBACK = function (
  userParam: TX_USERPARAM
): TX_THREADID; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


(*********************************************************************************************************************)

(**
  Callback used to join a thread.
    See txInitializeEyeX, TX_THREADINGMODEL

  @param threadId [in]:
    The id of the thread to join.

  @param userParam [in]:
    The user parameter provided by the TX_THREADINGMODEL structure.

  @return
    TX_TRUE if the thread was successfully joined. TX_FALSE on non existing thread.
 *)
TX_JOINTHREADCALLBACK = function (
  threadId: TX_THREADID;
  userParam: TX_USERPARAM
): TX_BOOL; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


(**
  Callback used to delete a thread.
    See txInitializeEyeX, TX_THREADINGMODEL

  @param threadId [in]:
    The id of the thread to be deleted.

  @param userParam [in]:
    The user parameter provided by the TX_THREADINGMODEL structure.

  @return
    TX_TRUE if the thread was successfully deleted, otherwise TX_FALSE.
 *)
TX_DELETETHREADCALLBACK = function (
  threadId: TX_THREADID;
  userParam: TX_USERPARAM
): TX_BOOL; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


(**
  Callback used to release a threading model or a logging model.
    See SetThreadingModel, SetLoggingModel

  @param userParam [in]:
    Normally used for capture outside the scope of the callback.
 *)
TX_DELETEMODELCALLBACK = procedure (
  userParam: TX_USERPARAM
); {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


(**
  Allocator function, used to override allocation of memory
    See SetCustomAllocator

  @param length [in]:
    Size in bytes of the requested memory block

  @return
    void
 *)
TX_ALLOCATORFUNCTION = procedure (
  length: TX_INTEGER
); {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


(**
  Callback for logging.
    If a custom logging model is set, see TX_LOGGINGMODEL, this callback will be invoked when a log message is
    written by the API.

  @param level [in]:
    The level of log message, see TX_LOGLEVEL for levels.

  @param scope [in]:
    A string token representing from which part the log message was originated.

  @param message [in]:
    The message to be logged.

  @param userParam [in]:
    The user parameter provided by the TX_LOGGINGMODEL structure.

  @return
    void
 *)
TX_LOGCALLBACK = procedure (
  level: TX_LOGLEVEL;
  scope: TX_CONSTSTRING;
  message: TX_CONSTSTRING;
  userParam: TX_USERPARAM
); {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


(**
  Function provided by the API when a job is scheduled.
    See TX_SCHEDULEJOBCALLBACK.

  @param jobParam [in]:
    The user parameter provided by the API when a job is scheduled.

  @return
    void
 *)
TX_PERFORMJOBFUNCTION = procedure (
  jobParam: TX_USERPARAM
); {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


(**
  Callback for scheduling a job.
    If a custom scheduling model is set, see TX_SCHEDULINGMODEL, this callback will be invoked when a job is to be
    scheduled.

  @param performJob [in]:
    The function to invoke when the job is to be performed.

  @param jobParam [in]:
    A parameter used to provide a context to the job.

  @param userParam [in]:
    The user parameter provided to the TX_SCHEDULINGMODEL.

  @return
    void
 *)
TX_SCHEDULEJOBCALLBACK = procedure (
  performJob: TX_PERFORMJOBFUNCTION;
  jobParam: TX_USERPARAM;
  userParam: TX_USERPARAM
); {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


 // Structs

  /// <summary>
  ///   Struct for a rectangle.
  /// </summary>
  TX_RECT = record
    /// <summary>
    ///   The X coordinate for the upper left corner of the rectangle.
    /// </summary>
    X: TX_REAL;
    /// <summary>
    ///   The Y coordinate for the upper left corner of the rectangle.
    /// </summary>
    Y: TX_REAL;
    /// <summary>
    ///   The width of the rectangle.
    /// </summary>
    Width: TX_REAL;
    /// <summary>
    ///   The height of the rectangle.
    /// </summary>
    Height: TX_REAL;
  end;
  TTxRect = TX_RECT;
  PTxRect = ^TTxRect;

  /// <summary>
  ///   Struct for 2D vector.
  /// </summary>
  TX_VECTOR2 = record
    /// <summary>
    ///   The X coordinate of the vector.
    /// </summary>
    X: TX_REAL;
    /// <summary>
    ///   The Y coordinate of the vector.
    /// </summary>
    Y: TX_REAL;
  end;
  TTxVector2 = TX_VECTOR2;
  PTxVector2 = ^TTxVector2;

  /// <summary>
  ///   Struct for 2D size.
  /// </summary>
  TX_SIZE2 = record
    /// <summary>
    ///   The width of the size.
    /// </summary>
    Width: TX_REAL;
    /// <summary>
    ///   The height of the size.
    /// </summary>
    Height: TX_REAL;
  end;
  TTxSize2 = TX_SIZE2;
  PTxSize2 = ^TTxSize2;


  /// <summary>
  ///   Struct for pannable behavior parameters.
  /// </summary>
  TX_PANNABLEPARAMS = record
    /// <summary>
    ///   Set to TX_FALSE - hands free panning is not yet implemented.
    /// </summary>
    IsHandsFreeEnabled: TX_BOOL;
    /// <summary>
    ///   The panning profile. See TX_PANNINGPROFILE.
    /// </summary>
    Profile: TX_PANNINGPROFILE;
    /// <summary>
    ///   Currently not used.
    /// </summary>
    PeakVelocity: TX_REAL;
    /// <summary>
    ///   Flags specifying which pan directions are currently possible. See
    ///   TX_PANDIRECTION. <br />Correct pan direction flags are needed for
    ///   panning to work properly.
    /// </summary>
    PanDirectionsAvailable: TX_PANDIRECTION;
  end;
  TTxPannableparams = TX_PANNABLEPARAMS;
  PTxPannableparams = ^TTxPannableparams;

  /// <summary>
  ///   Struct for pannable pan event parameters.
  /// </summary>
  TX_PANNABLEPANEVENTPARAMS = record
    /// <summary>
    ///   The X velocity for the pan. In pixels per second.
    /// </summary>
    PanVelocityX: TX_REAL;
    /// <summary>
    ///   The Y velocity for the pan. In pixels per second.
    /// </summary>
    PanVelocityY: TX_REAL;
  end;
  TTxPannablepaneventparams = TX_PANNABLEPANEVENTPARAMS;
  PTxPannablepaneventparams = ^TTxPannablepaneventparams;

 /// <summary>
  ///   Struct for pannable step event parameters.
  /// </summary>
  TX_PANNABLESTEPEVENTPARAMS = record
    /// <summary>
    ///   The step length on the X axis in pixels.
    /// </summary>
    PanStepX: TX_REAL;
    /// <summary>
    ///   The step length on the Y axis in pixels.
    /// </summary>
    PanStepY: TX_REAL;
    /// <summary>
    ///   The amount of time in seconds during which the step should be
    ///   performed.
    /// </summary>
    PanStepDuration: TX_REAL;
  end;
  TTxPannablestepeventparams = TX_PANNABLESTEPEVENTPARAMS;
  PTxPannablestepeventparams = ^TTxPannablestepeventparams;

  /// <summary>
  ///   Struct for pannable hands free event parameters.
  /// </summary>
  TX_PANNABLEHANDSFREEEVENTPARAMS = record
    /// <summary>
    ///   Specifies if hands free panning is enabled or not.
    /// </summary>
    HandsFreeEnabled: TX_BOOL;
  end;
  TTxPannablehandsfreeeventparams = TX_PANNABLEHANDSFREEEVENTPARAMS;
  PTxPannablehandsfreeeventparams = ^TTxPannablehandsfreeeventparams;

  /// <summary>
  ///   Struct for activatable behavior parameters.
  /// </summary>
  TX_ACTIVATABLEPARAMS = record
    /// <summary>
    ///   Specifies if tentative focus should be enabled.
    /// </summary>
    EnableTentativeFocus: TX_BOOL;
    /// <summary>
    ///   Specifies if small item detection should be enabled.
    /// </summary>
    EnableSmallItemDetection: TX_BOOL;
  end;
  TTxActivatableparams = TX_ACTIVATABLEPARAMS;
  PTxActivatableparams = ^TTxActivatableparams;

  /// <summary>
  ///   Struct for gaze aware parameters.
  /// </summary>
  TX_GAZEAWAREPARAMS = record
    /// <summary>
    ///   Specifies the gaze aware mode. See TX_GAZEAWAREMODE.
    /// </summary>
    GazeAwareMode: TX_GAZEAWAREMODE;
    /// <summary>
    ///   Specifies the amount of time in milliseconds that the user has to
    ///   look at an interactor before a gaze aware event is sent. This value
    ///   only has an effect if the mode is set to TX_GAZEAWAREMODE_DELAYED. <br />
    /// </summary>
    DelayTime: TX_REAL;
  end;
  TTxGazeawareparams = TX_GAZEAWAREPARAMS;
  PTxGazeawareparams = ^TTxGazeawareparams;

  /// <summary>
  ///   Struct for gaze aware event parameters.
  /// </summary>
  TX_GAZEAWAREEVENTPARAMS = record
    /// <summary>
    ///   Specifies if the interactor currently has gaze on it.
    /// </summary>
    HasGaze: TX_BOOL;
  end;
  TTxGazeawareeventparams = TX_GAZEAWAREEVENTPARAMS;
  PTxGazeawareeventparams = ^TTxGazeawareeventparams;

  /// <summary>
  ///   Struct for activation focus changed Params.
  /// </summary>
  TX_ACTIVATIONFOCUSCHANGEDEVENTPARAMS = record
    /// <summary>
    ///   Specifies if the interactor currently has tentative activation focus.
    /// </summary>
    HasTentativeActivationFocus: TX_BOOL;
    /// <summary>
    ///   Specifies if the interactor currently has activation focus.
    /// </summary>
    HasActivationFocus: TX_BOOL;
  end;
  TTxActivationfocuschangedeventparams = TX_ACTIVATIONFOCUSCHANGEDEVENTPARAMS;
  PTxActivationfocuschangedeventparams = ^TTxActivationfocuschangedeventparams;

  /// <summary>
  ///   Struct for gaze point data behavior parameters.
  /// </summary>
  TX_GAZEPOINTDATAPARAMS = record
    /// <summary>
    ///   Specifies the gaze point data mode. See TX_GAZEPOINTDATAMODE.
    /// </summary>
    GazePointDataMode: TX_GAZEPOINTDATAMODE;
  end;
  TTxGazepointdataparams = TX_GAZEPOINTDATAPARAMS;
  PTxGazepointdataparams = ^TTxGazepointdataparams;

  /// <summary>
  ///   Struct for fixation behavior parameters.
  /// </summary>
  TX_FIXATIONDATAPARAMS = record
    /// <summary>
    ///   Specifies the fixation data mode. See TX_FIXATIONDATAMODE.
    /// </summary>
    FixationDataMode: TX_FIXATIONDATAMODE;
  end;
  TTxFixationdataparams = TX_FIXATIONDATAPARAMS;
  PTxFixationdataparams = ^TTxFixationdataparams;

  /// <summary>
  ///   Struct for fixation behavior event parameters.
  /// </summary>
  TX_FIXATIONDATAEVENTPARAMS = record
    /// <summary>
    ///   The fixation data mode. See TX_FIXATIONDATAMODE.
    /// </summary>
    FixationDataMode: TX_FIXATIONDATAMODE;
    /// <summary>
    ///   The type of fixation event. See TX_FIXATIONDATAEVENTTYPE.
    /// </summary>
    EventType: TX_FIXATIONDATAEVENTTYPE;
    /// <summary>
    ///   For TX_FIXATIONDATAEVENTTYPE_BEGIN, this is the time when the
    ///   fixation started, in milliseconds. <br />For
    ///   TX_FIXATIONDATAEVENTTYPE_END, this is the time when the fixation
    ///   ended, in milliseconds. <br />For TX_FIXATIONDATAEVENTTYPE_DATA, the
    ///   timestamp for the filtered gaze point provided within the current
    ///   fixation, when the filter was applied, in milliseconds. <br />
    /// </summary>
    Timestamp: TX_REAL;
    /// <summary>
    ///   The current X coordinate of the fixation in pixels. For begin and end
    ///   events will reflect where the fixation began or ended. <br />
    /// </summary>
    X: TX_REAL;
    /// <summary>
    ///   The current Y coordinate of the fixation in pixels. For begin and end
    ///   events will reflect where the fixation began or ended. <br />
    /// </summary>
    Y: TX_REAL;
  end;
  TTxFixationdataeventparams = TX_FIXATIONDATAEVENTPARAMS;
  PTxFixationdataeventparams = ^TTxFixationdataeventparams;

  /// <summary>
  ///   Struct for gaze point data behavior event parameters.
  /// </summary>
  TX_GAZEPOINTDATAEVENTPARAMS = record
    /// <summary>
    ///   The gaze point data mode. See TX_GAZEPOINTDATAMODE.
    /// </summary>
    GazePointDataMode: TX_GAZEPOINTDATAMODE;
    /// <summary>
    ///   For TX_GAZEPOINTDATAMODE_LIGHTLYFILTERED this is the point in time
    ///   when the filter was applied, in milliseconds. <br />For
    ///   TX_GAZEPOINTDATAMODE_UNFILTERED this is the point in time time when
    ///   gazepoint was captured, in milliseconds. <br />
    /// </summary>
    Timestamp: TX_REAL;
    /// <summary>
    ///   The X coordinate of the gaze point in pixels.
    /// </summary>
    X: TX_REAL;
    /// <summary>
    ///   The Y coordinate of the gaze point in pixels.
    /// </summary>
    Y: TX_REAL;
  end;
  TTxGazepointdataeventparams = TX_GAZEPOINTDATAEVENTPARAMS;
  PTxGazepointdataeventparams = ^TTxGazepointdataeventparams;


  /// <summary>
  ///   Struct for eye position data behavior event parameters. <br /><br />The
  ///   components of the eye vectors are the relative position of the eyes
  ///   from the center of the screen in millimeters on each axis. <br />
  /// </summary>
  TX_EYEPOSITIONDATAEVENTPARAMS = record
    /// <summary>
    ///   The point in time when the eye position was captured, in
    ///   milliseconds.
    /// </summary>
    Timestamp: TX_REAL;
    /// <summary>
    ///   Specifies if the data for the left eye is valid.
    /// </summary>
    HasLeftEyePosition: TX_BOOL;
    /// <summary>
    ///   Specifies if the data for the right eye is valid.
    /// </summary>
    HasRightEyePosition: TX_BOOL;
    /// <summary>
    ///   The X coordinate of the left eye in millimeters.
    /// </summary>
    LeftEyeX: TX_REAL;
    /// <summary>
    ///   The Y coordinate of the left eye in millimeters.
    /// </summary>
    LeftEyeY: TX_REAL;
    /// <summary>
    ///   The Z coordinate of the left eye in millimeters.
    /// </summary>
    LeftEyeZ: TX_REAL;
    /// <summary>
    ///   The X coordinate of the left eye normalized in the track box.
    /// </summary>
    LeftEyeXNormalized: TX_REAL;
    /// <summary>
    ///   The Y coordinate of the left eye normalized in the track box. <br />
    /// </summary>
    LeftEyeYNormalized: TX_REAL;
    /// <summary>
    ///   The Z coordinate of the left eye normalized in the track box.
    /// </summary>
    LeftEyeZNormalized: TX_REAL;
    /// <summary>
    ///   The X coordinate of the right eye in millimeters.
    /// </summary>
    RightEyeX: TX_REAL;
    /// <summary>
    ///   The Y coordinate of the right eye in millimeters. <br />
    /// </summary>
    RightEyeY: TX_REAL;
    /// <summary>
    ///   The Z coordinate of the right eye in millimeters.
    /// </summary>
    RightEyeZ: TX_REAL;
    /// <summary>
    ///   The X coordinate of the right eye normalized in the track box.
    /// </summary>
    RightEyeXNormalized: TX_REAL;
    /// <summary>
    ///   The Y coordinate of the right eye normalized in the track box.
    /// </summary>
    RightEyeYNormalized: TX_REAL;
    /// <summary>
    ///   The Y coordinate of the right eye normalized in the track box.
    /// </summary>
    RightEyeZNormalized: TX_REAL;
  end;
  TTxEyepositiondataeventparams = TX_EYEPOSITIONDATAEVENTPARAMS;
  PTxEyepositiondataeventparams = ^TTxEyepositiondataeventparams;

  /// <summary>
  ///   Struct for the threading model.
  /// </summary>
  TX_THREADINGMODEL = record
    /// <summary>
    ///   Callback function used to create a thread. See
    ///   TX_CREATETHREADCALLBACK.
    /// </summary>
    CreateThread: TX_CREATETHREADCALLBACK;
    /// <summary>
    ///   Callback function used to get the id of the current (calling) thread.
    ///   See TX_GETCURRENTTHREADIDCALLBACK.
    /// </summary>
    GetCurrentThreadId: TX_GETCURRENTTHREADIDCALLBACK;
    /// <summary>
    ///   Callback function used to join a thread. See TX_JOINTHREADCALLBACK.
    /// </summary>
    JoinThread: TX_JOINTHREADCALLBACK;
    /// <summary>
    ///   Callback function used to delete a thread. See
    ///   TX_DELETETHREADCALLBACK.
    /// </summary>
    DeleteThread: TX_DELETETHREADCALLBACK;
    /// <summary>
    ///   Callback function used to release the threading model.
    /// </summary>
    DeleteModel: TX_DELETEMODELCALLBACK;
    /// <summary>
    ///   User parameter which will be passed to the functions.
    /// </summary>
    UserParam: TX_USERPARAM;
  end;
  PTXTHREADINGMODEL = ^TX_THREADINGMODEL;

  /// <summary>
  ///   Struct for the logging model.
  /// </summary>
  TX_LOGGINGMODEL = record
    /// <summary>
    ///   Specifies which log targets to use. See TX_LOGTARGET. <br />
    /// </summary>
    Targets: TX_LOGTARGET;
    /// <summary>
    ///   Callback function used to write a custom log message. See
    ///   TX_LOGCALLBACK. <br />
    /// </summary>
    Log: TX_LOGCALLBACK;
    /// <summary>
    ///   Callback function used to release the logging model.
    /// </summary>
    DeleteModel: TX_DELETEMODELCALLBACK;
    /// <summary>
    ///   User parameter which will be passed to the custom log function. <br />
    /// </summary>
    UserParam: TX_USERPARAM;
  end;
  PTXLOGGINGMODEL = ^TX_LOGGINGMODEL;

(**
  Struct for the scheduling model.

  @field Mode:
     Specifies which scheduling mode to use. See TX_SCHEDULINGMODE.

  @field Schedule:
     Callback function schedule a work item. See TX_SCHEDULEJOBCALLBACK.

  @field DeleteModel:
     Callback function used to release the logging model.

  @field UserParam:
    User parameter which will be passed to the custom schedule function.
 *)
  TX_SCHEDULINGMODEL = record
    Mode: TX_SCHEDULINGMODE;
    ScheduleJob: TX_SCHEDULEJOBCALLBACK;
    DeleteModel: TX_DELETEMODELCALLBACK;
    UserParam: TX_USERPARAM;
  end;
  TTxSchedulingmodel = TX_SCHEDULINGMODEL;
  PTxSchedulingmodel = ^TTxSchedulingmodel;

// EyeXEnv.h


/// <summary>
///   Initializes the Tobii EyeX client environment. <br />This function must
///   be called prior to any other in the API, except txGetEyeXAvailability and
///   txEnableMonoCallbacks. A client can choose to override the default memory
///   model, threading model and logging model by supplying custom models to
///   this function. <br />
/// </summary>
/// <param name="flags">
///   Specifies which components to override.
/// </param>
/// <param name="LoggingModel">
///   A pointer to a TX_LOGGINGMODEL which will override the default model.
///   This argument can be NULL to use the default logging model. <br />
/// </param>
/// <param name="ThreadingModel">
///   A pointer to a TX_THREADINGMODEL which will override the default model.
///   This argument can be NULL to use the default threading model. Any
///   non-NULL value is for internal use only. <br />
/// </param>
/// <param name="SchedulingModel">
///   A pointer to a TX_SCHEDULINGMODEL which will override the default model. <br />
///   This argument can be NULL to use the default scheduling model. Any
///   non-NULL value is for internal use only. <br />
/// </param>
/// <param name="pMemoryModel">
///   Reserved for future use.
/// </param>
/// <returns>
///   TX_RESULT_OK: The client environment was successfully initialized. <br />
///   TX_RESULT_INVALIDARGUMENT: An invalid argument was passed to the
///   function. <br />TX_RESULT_EYEXALREADYINITIALIZED: The EyeX client
///   environment is already initialized.
/// </returns>
function txInitializeEyeX(
  flags: TX_EYEXCOMPONENTOVERRIDEFLAGS;
  pLoggingModel: PTXLOGGINGMODEL;
  pThreadingModel: PTXTHREADINGMODEL;
  pSchedulingModel: PTXSCHEDULINGMODEL;
  pMemoryModel: Pointer
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Uninitializes the EyeX client environment. <br />If any context is still
///   active this call will fail.
/// </summary>
/// <returns>
///   TX_RESULT_OK: The client environment was successfully uninitialized. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The client environment is not initialized. <br />
///   TX_RESULT_EYEXSTILLINUSE: The EyeX client environment is still in use. <br />
/// </returns>
function txUninitializeEyeX(): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Checks if the EyeX client environment has been initialized.
/// </summary>
/// <param name="Initialized">
///   A pointer to a TX_BOOL which will be set to true if the environment is
///   initialized and false otherwise. <br />Must not be NULL.
/// </param>
/// <returns>
///   TX_RESULT_OK: The operation was successful.
/// </returns>
function txIsEyeXInitialized(
  var Initialized: TX_BOOL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Writes a message using the internal logging model. <br />This method is
///   typically not intended for end users but rather for the different
///   language bindings to have a common way of utilizing the logging model. <br />
/// </summary>
/// <param name="level">
///   The log level for this message.
/// </param>
/// <param name="scope">
///   The scope for this message.
/// </param>
/// <param name="txmessage">
///   The log message itself.
/// </param>
/// <returns>
///   TX_RESULT_OK: The message was successfully written to the log. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txWriteLogMessage(
  level: TX_LOGLEVEL;
  scope: TX_CONSTSTRING;
  txmessage: TX_CONSTSTRING
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets a hook that notifies when an invalid argument has been passed to any
///   of the API function. <br />This function should typically only be used
///   for testing purposes.
/// </summary>
/// <param name="handler">
///   The callback to be invoked when an invalid argument is detected.
/// </param>
/// <param name="userParam">
///   A TX_USERPARAM which will be provided as a parameter to the callback. <br />
///   Can be NULL and will in this case be ignored.
/// </param>
/// <returns>
///   TX_RESULT_OK: The invalid argument handler was successful set.
/// </returns>
function txSetInvalidArgumentHandler(
  handler: TX_INVALIDARGUMENTCALLBACK;
  userParam: TX_USERPARAM
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Prepares the EyeX client library for use with the Mono .NET runtime:
///   before a callback function is invoked, the thread on which the callback
///   will be made is attached to a mono domain, and the thread is detached
///   again when the callback function returns. Mono requires that any threads
///   calling managed code be attached for garbage collection and soft
///   debugging to work properly. <br /><br />This function must be called
///   prior to any other in the API, and from a managed thread. The subsequent
///   callback invocations will be attached to the same mono domain as the
///   caller thread. <br /><br />Note that Mono callbacks cannot be used in
///   combination with a custom threading model.
/// </summary>
/// <param name="monoModuleName">
///   The name of the Mono runtime module (dll). Typically "mono".
/// </param>
/// <returns>
///   TX_RESULT_OK: The mono callbacks were successfully enabled. <br />
///   TX_RESULT_INVALIDARGUMENT: The Mono module name could not be used to
///   resolve the necessary Mono functions. <br />
/// </returns>
function txEnableMonoCallbacks(
  monoModuleName: TX_CONSTSTRING
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Gets the availability of the EyeX Engine.
/// </summary>
/// <param name="EyeXAvailability">
///   The availability of EyeX Engine.
/// </param>
/// <returns>
///   TX_RESULT_OK: The status was fetched successfully. <br />
///   TX_RESULT_INVALIDARGUMENT: An invalid argument was supplied.
/// </returns>
function txGetEyeXAvailability(
  var EyeXAvailability: TX_EYEXAVAILABILITY
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


 // Copyright 2013-2014 Tobii Technology AB. All rights reserved.
 // EyeXContext.h

/// <summary>
///   Creates a new context. <br />A context represents an environment in which
///   the interaction objects live and a single connection to the client.
/// </summary>
/// <param name="hContext">
///   A TX_CONTEXTHANDLE variable which will be set to the context. <br />Must
///   not be NULL. <br />
/// </param>
/// <param name="trackObjects">
///   Specifies if objects owned by this context should be tracked. <br />
///   Specifying TX_TRUE will give more information on leaking objects when
///   shutting down but comes with a performance hit and is not recommended for
///   production builds. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The context was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
/// <remarks>
///   Interaction objects may not be shared between contexts. A context must be
///   cleaned up using txReleaseContext to avoid leaks.
/// </remarks>
function txCreateContext(
  var hContext: TX_CONTEXTHANDLE;
  trackObjects: TX_BOOL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Releases an existing context. <br />If not all objects has been released
///   prior to this call then TX_RESULT_OBJECTLEAKAGE will be returned and the
///   context will not be released. The leaking objects may be retreived using
///   txGetTrackedObjects. <br />
/// </summary>
/// <param name="hContext">
///   A TX_CONTEXTHANDLE holding the context. <br />If the context is
///   successfully released the value of this pointer will be set to NULL.
/// </param>
/// <returns>
///   TX_RESULT_OK: The context was successfully released. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDTHREAD: Attempted to call
///   the function from a callback thread. <br />
/// </returns>
/// <remarks>
///   Must not be NULL.
/// </remarks>
function txReleaseContext(
  var hContext: TX_CONTEXTHANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Shuts down a context. <br />This call will block until the context has
///   been properly shut down or until the specified timeout has passed. <br />
///   If any interaction objects are still alive when a context is being shut
///   down this call will block for the specified timeout waiting for other
///   threads to release the objects. If any objects are still not released
///   after this amount of time then TX_RESULT_OBJECTLEAKAGE will be returned.
///   The leaking objects may be retreived using txGetTrackedObjects. <br />
/// </summary>
/// <param name="hContext">
///   A TX_CONTEXTHANDLE variable holding the context. <br />Must not be NULL.
/// </param>
/// <param name="cleanupTimeout">
///   The amount of time in millseconds to wait for objects to be released. <br />
///   Use the constant TX_CLEANUPTIMEOUT_DEFAULT for a default timeout of 500
///   ms. Use the special value TX_CLEANUPTIMEOUT_FORCEIMMEDIATE to shut down
///   the context immediately, without checking for leaking objects. <br />
/// </param>
/// <param name="logLeakingObjectsInfo">
///   Specifies if information about the leaking objects should be logged. To
///   get full details on leaking objects the context must be set up to track
///   objects when created. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The context was successfully shut down. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_OBJECTLEAKAGE: All interaction
///   objects have not been released properly. The context was not deleted. <br />
///   TX_RESULT_INVALIDTHREAD: Attempted to call the function from a callback
///   thread. <br />
/// </returns>
function txShutdownContext(
  hContext: TX_CONTEXTHANDLE;
  cleanupTimeout: TX_INTEGER;
  logLeakingObjectsInfo: TX_BOOL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Sets the name of a context. This name will only be used when logging
///   messages. This method is typically in debugging scenarios with multiple
///   contexts. <br />
/// </summary>
/// <param name="hContext">
///   A TX_CONTEXTHANDLE for the context on which to set the name. <br />Must
///   not be NULL. <br />
/// </param>
/// <param name="name">
///   The name of the context.
/// </param>
/// <returns>
///   TX_RESULT_OK: The name of the context was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetContextName(
  hContext: TX_CONTEXTHANDLE;
  name: TX_CONSTSTRING
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Sets custom params which gives the context different behaviors. For
///   internal use only.
/// </summary>
/// <param name="hContext">
///   A TX_CONTEXTHANDLE for the context on which to set the parameters. <br />
///   Must not be NULL.
/// </param>
/// <param name="hParams">
///   A TX_CONSTHANDLE to an object containing the params.
/// </param>
/// <returns>
///   TX_RESULT_OK: The params was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetContextParams(
  hContext: TX_CONTEXTHANDLE;
  hParams: TX_CONSTHANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Gets the name of a context.
/// </summary>
/// <param name="hContext">
///   A TX_CONTEXTHANDLE holding the context. <br />Must not be NULL. <br />
/// </param>
/// <param name="pName">
///   A TX_STRING to which the context name will be copied. <br />Must be at
///   least the size of the context name. Can be NULL to only get the size of
///   the context name. <br />
/// </param>
/// <param name="NameSize">
///   A TX_SIZE variable which will be set the size of the context name. <br />
///   Must not be NULL.
/// </param>
/// <returns>
///   TX_RESULT_OK: The context name or the required size of the string was
///   successfully retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT:
///   An invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of pName is invalid (*pNameSize
///   will be set to the required size). <br />
/// </returns>
function txGetContextName(
  hContext: TX_CONTEXTHANDLE;
  pName: TX_STRING;
  var NameSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets all the objects that currently tracked by a context. This requires
///   the context to be set up to track objects.
/// </summary>
/// <param name="hContext">
///   A TX_CONSTCONTEXTHANDLE to the context from which to get the tracked
///   objects. <br /><br />Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="phObjects">
///   An array of TX_HANDLEs to which the property handles will be copied.
///   These handles must be released using txReleaseObject to avoid leaks. <br /><br />
///   Can be nil to only retrieve the size. <br />
/// </param>
/// <param name="ObjectsSize">
///   A TX_SIZE record which will be set to the number of objects. <br />Must
///   not be NULL. <br />The value must be 0 if hObjects is nil. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The objects or required sie of the buffer were successfully
///   retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX client
///   environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT: An
///   invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of the buffer was to small.
///   (*pObjectSize will be set to the required size.) <br />
///   TX_RESULT_OBJECTTRACKINGNOTENABLED: The specified context has not been
///   set up to track objects. <br />
/// </returns>
function txGetTrackedObjects(
  hContext: TX_CONSTCONTEXTHANDLE;
  phObjects: PTXHANDLE;
  var ObjectsSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Enables the connection to the client. <br />This method must be called to
///   start the communication between the client and server. <br />Once the
///   connection has been enabled the client will attempt to always keep it
///   alive. If the connection to the client for some reason goes down, the
///   client will immediately attemt to reconnect. <br />See
///   txRegisterConnectionStateChangedHandler to get notified of the current
///   connection state. <br />
/// </summary>
/// <param name="hContext">
///   A TX_CONTEXTHANDLE to the context which should get its connection
///   enabled. <br />Must not be TX_EMPTY_HANDLE.
/// </param>
/// <returns>
///   TX_RESULT_OK: The connection was successfully enabled (does not mean that
///   a connection to the client has been established). <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txEnableConnection(
  hContext: TX_CONTEXTHANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Disables the connection to the client.
/// </summary>
/// <param name="hContext">
///   A TX_CONTEXTHANDLE to the context which should get its connection
///   disabled. <br />Must not be TX_EMPTY_HANDLE.
/// </param>
/// <returns>
///   <para>
///     TX_RESULT_OK: The connection was successfully disabled.
///   </para>
///   <para>
///     TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///     initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///     passed to the function.
///   </para>
/// </returns>
function txDisableConnection(
  hContext: TX_CONTEXTHANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Gets the current connection state of the context.
/// </summary>
/// <param name="hContext">
///   A TX_CONSTCONTEXTHANDLE to the context from which to retrieve the
///   connection state. <br />Must not be TX_EMPTY_HANDLE. <br />
/// </param>
/// <param name="ConnectionState">
///   A TX_CONNECTIONSTATE which will get the current connection state. <br />
///   Must not be NULL. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The current connection state was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txGetConnectionState(
  hContext: TX_CONSTCONTEXTHANDLE;
  var ConnectionState: TX_CONNECTIONSTATE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Registers a callback which will be invoked when the connection state
///   changes. Use txEnableConnection <br />to initiate a connection to the
///   engine. <br />
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context on which to register the callback.
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE. <br />
///   </para>
/// </param>
/// <param name="Ticket">
///   A TX_TICKET which will represent this registration. <br />This ticket
///   should be used for unregistration.
/// </param>
/// <param name="handler">
///   A TX_CONNECTIONSTATECHANGEDCALLBACK which will be called when the
///   connection state changes. <br />Must not be NULL.
/// </param>
/// <param name="userParam">
///   A TX_USERPARAM which will be provided as a parameter to the callback.<br />
///   Can be NULL and will in this case be ignored. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The callback was successfully registered. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDTHREAD: Attempted to call
///   the function from a callback thread. <br />
/// </returns>
function txRegisterConnectionStateChangedHandler(
  hContext: TX_CONTEXTHANDLE;
  var Ticket: TX_TICKET;
  handler: TX_CONNECTIONSTATECHANGEDCALLBACK;
  userParam: TX_USERPARAM
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Unregisters a callback previously registered for connection state
///   changes. <br />This function may not be called on a callback thread. <br />
/// </summary>
/// <param name="hContext">
///   A TX_CONTEXTHANDLE to the context on which to unregister the callback. <br />
///   Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="ticket">
///   A TX_TICKET which represents the registration. <br />Must not be
///   TX_INVALID_TICKET. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The callback was successfully unregistered. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: A registration for the
///   specified ticket could not be found. <br />TX_RESULT_INVALIDTHREAD:
///   Attempted to call the function from a callback thread.
/// </returns>
function txUnregisterConnectionStateChangedHandler(
  hContext: TX_CONTEXTHANDLE;
  ticket: TX_TICKET
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Registers a callback to be invoked when a message of a specific type
///   arrives. <br />This function may not be called on a callback thread.
/// </summary>
/// <param name="hContext">
///   A TX_CONTEXTHANDLE to the context on which to register the callback. <br />
///   Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="Ticket">
///   A pointer to a TX_TICKET which will represent this registration. <br />
///   This ticket should be used for unregistration. <br />Must not be NULL.
/// </param>
/// <param name="messageType">
///   The TX_MESSAGETYPE for which to register the callback.
/// </param>
/// <param name="hOptions">
///   <para>
///     A TX_HANDLE to an interaction object containing the options for this
///     registration. <br />
///   </para>
///   <para>
///     The following options should be provided for the following message
///     types: <br /><br />TX_MESSAGETYPE_QUERY :- TX_LITERAL_PROCESSID: The
///     id of the process for which to get queries. <br />
///   </para>
/// </param>
/// <param name="handler">
///   <para>
///     A TX_ASYNCDATACALLBACK which will be called when a message of the
///     specified type arrives.
///   </para>
///   <para>
///     When this callback is invoked it is passed a parameter hAsyncData
///     which contains the message as its content. The content of the async
///     data can be retrieved using txGetAsyncDataContent. That handle to the
///     async data must NOT be released as it is automatically released by
///     the API when the callback returns. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <param name="userParam">
///   A TX_USERPARAM which will be provided as a parameter to the callback. <br />
///   Can be NULL and will in this case be ignored.
/// </param>
/// <returns>
///   TX_RESULT_OK: The callback was successfully registered. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDTHREAD: Attempted to call
///   the function from a callback thread.
/// </returns>
function txRegisterMessageHandler(
  hContext: TX_CONTEXTHANDLE;
  var Ticket: TX_TICKET;
  messageType: TX_MESSAGETYPE;
  hOptions: TX_HANDLE;
  handler: TX_ASYNCDATACALLBACK;
  userParam: TX_USERPARAM
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Unregisters a callback previously registered as a message handler. <br />
/// </summary>
/// <param name="hContext">
///   A TX_CONTEXTHANDLE to the context on which to unregister the callback. <br />
///   Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="ticket">
///   A TX_TICKET which represents the registration. <br />Must not be
///   TX_INVALID_TICKET <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The callback was successfully unregistered. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: A registration for the
///   specified ticket could not be found. <br />TX_RESULT_INVALIDTHREAD:
///   Attempted to call the function from a callback thread. <br />
/// </returns>
function txUnregisterMessageHandler(
  hContext: TX_CONTEXTHANDLE;
  ticket: TX_TICKET
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   <para>
///     Registers a query handler.
///   </para>
///   <para>
///     This will setup a subscription to receive interactor queries from the
///     engine. This is a specialization of txRegisterMessageHandler. <br />
///   </para>
/// </summary>
/// <param name="hContext">
///   A TX_CONTEXTHANDLE to the context on which to register the callback. Must
///   not be TX_EMPTY_HANDLE. <br />
/// </param>
/// <param name="Ticket">
///   A TX_TICKET which will represent this registration. <br />This ticket
///   should be used for unregistration. <br />Must not be NULL. <br />
/// </param>
/// <param name="handler">
///   <para>
///     A TX_ASYNCDATACALLBACK which will be called when a query arrives. <br />
///   </para>
///   <para>
///     When this callback is invoked it is passed a parameter hAsyncData
///     which contains the query as its content. The content of the async
///     data can be retrieved using txGetAsyncDataContent(). That handle to
///     the async data must NOT be released as it is automatically released
///     by the API when the callback returns. <br />
///   </para>
///   <para>
///     Must not be NULL. <br />
///   </para>
/// </param>
/// <param name="userParam">
///   A TX_USERPARAM which will be provided as a parameter to the callback.
/// </param>
/// <returns>
///   TX_RESULT_OK: The Query Handler was successfully registered. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDTHREAD: Attempted to call
///   the function from a callback thread. <br />
/// </returns>
function txRegisterQueryHandler(
  hContext: TX_CONTEXTHANDLE;
  var Ticket: TX_TICKET;
  handler: TX_ASYNCDATACALLBACK;
  userParam: TX_USERPARAM
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Unregisters a previously registered query handler callback <br />This is
///   a specialization of txUnregisterMessageHandler. <br />
/// </summary>
/// <param name="hContext">
///   A TX_CONTEXTHANDLE to the context on which to unregister the callback. <br />
///   Must not be TX_EMPTY_HANDLE. <br />
/// </param>
/// <param name="ticket">
///   A TX_TICKET which represents the registration. <br />Must not be
///   TX_INVALID_TICKET <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The callback was successfully unregistered. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: A registration for the
///   specified ticket could not be found. <br />TX_RESULT_INVALIDTHREAD:
///   Attempted to call the function from a callback thread. <br />
/// </returns>
function txUnregisterQueryHandler(
  hContext: TX_CONTEXTHANDLE;
  ticket: TX_TICKET
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Registers an event handler. This will setup a subscription to receive
///   interaction events from the engine.
/// </summary>
/// <param name="hContext">
///   A TX_CONTEXTHANDLE to the context on which to register the callback. <br />
///   Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="Ticket">
///   A pointer to a TX_TICKET which will represent this registration. This
///   ticket should be used for unregistration. <br />Must not be NULL. <br />
/// </param>
/// <param name="handler">
///   <para>
///     A TX_ASYNCDATACALLBACK which will be called when an event arrives.
///   </para>
///   <para>
///     When this callback is invoked it is passed a parameter hAsyncData
///     which contains the event as its content. The content of the async
///     data can be retrieved using txGetAsyncDataContent(). <br />That
///     handle to the async data must NOT be released as it is automatically
///     released by the API when the callback returns. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <param name="userParam">
///   A TX_USERPARAM which will be provided as a parameter to the callback.
/// </param>
/// <returns>
///   TX_RESULT_OK: The event handler was successfully registered. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDTHREAD: Attempted to call
///   the function from a callback thread.
/// </returns>
function txRegisterEventHandler(
  hContext: TX_CONTEXTHANDLE;
  var Ticket: TX_TICKET;
  handler: TX_ASYNCDATACALLBACK;
  userParam: TX_USERPARAM
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Unregisters a previously registered event handler callback <br />This is
///   a specialization of txUnregisterMessageHandler.
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context on which to unregister the
///     callback.
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="ticket">
///   <para>
///     A TX_TICKET which represents the registration. <br />
///   </para>
///   <para>
///     Must not be TX_INVALID_TICKET <br />
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The callback was successfully unregistered. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: A registration for the
///   specified ticket could not be found. <br />TX_RESULT_INVALIDTHREAD:
///   Attempted to call the function from a callback thread.
/// </returns>
function txUnregisterEventHandler(
  hContext: TX_CONTEXTHANDLE;
  ticket: TX_TICKET
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Performs all jobs scheduled by the API on the thread that calls this
///   function. <br />This call will block until all jobs have been performed.
///   For internal use only. <br />
/// </summary>
/// <param name="hContext">
///   A TX_CONTEXTHANDLE to the context which jobs should be performed.
/// </param>
/// <returns>
///   TX_RESULT_OK: The callback was successfully unregistered. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDSCHEDULINGMODE: The
///   scheduling mode must be set TX_SCHEDULINGMODE_USERFRAME for this call to
///   be valid. <br />
/// </returns>
function txPerformScheduledJobs(
  hContext: TX_CONTEXTHANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


//* EyeXObject.h

/// <summary>
///   Gets the context to which a specified interaction object belongs. <br />
///   Unlike interaction objects, this handle does not need to be released.
/// </summary>
/// <param name="hObject">
///   <para>
///     A TX_CONSTHANDLE to the interaction object for which the context
///     should be retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hContext">
///   <para>
///     A TX_CONSTCONTEXTHANDLE which will be set to the context. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The context was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. TX_RESULT_INVALIDARGUMENT: An invalid argument was passed to
///   the function.
/// </returns>
function txGetContext(
  hObject: TX_CONSTHANDLE;
  var hContext: TX_CONTEXTHANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Gets the TX_INTERACTIONOBJECTTYPE of an interaction object.
/// </summary>
/// <param name="hObject">
///   <para>
///     A TX_CONSTHANDLE to the interaction object for which to get the
///     TX_INTERACTIONOBJECTTYPE. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hObjectType">
///   <para>
///     A TX_INTERACTIONOBJECTTYPE which will be set to the type of the
///     object. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The TX_INTERACTIONOBJECTTYPE of the object was successfully
///   retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX client
///   environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT: An
///   invalid argument was passed to the function.
/// </returns>
function txGetObjectType(
  hObject: TX_CONSTHANDLE;
  var hObjectType: TX_INTERACTIONOBJECTTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the type name of an interaction object.
/// </summary>
/// <param name="hObject">
///   <para>
///     A TX_CONSTHANDLE to the interaction object for which to get the type
///     name. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="pObjectTypeName">
///   <para>
///     A TX_STRING to which the type name will be copied. <br />The string
///     will be null terminated. <br />
///   </para>
///   <para>
///     May be NULL to only retrieve the size.
///   </para>
/// </param>
/// <param name="ObjectTypeNameSize">
///   <para>
///     A TX_SIZE which should contain the size of the pObjectTypeName
///     string. The size will be set to the current number of characters in
///     the type name + 1. <br />
///   </para>
///   <para>
///     Must not be NULL. <br />
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The type name of the object or size of the string was
///   successfully retreived. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT:
///   An invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of the pObjectTypeName was to
///   small. (*pObjectTypeNameSize will be set to the required size.) <br />
/// </returns>
function txGetObjectTypeName(
  hObject: TX_CONSTHANDLE;
  pObjectTypeName: TX_STRING;
  var ObjectTypeNameSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Releases an interaction object. The object reference count will be
///   decreased by one. If the reference count hits 0 the object will be
///   destroyed. The handle will be set to TX_EMPTY_HANDLE. <br />
/// </summary>
/// <param name="hObject">
///   <para>
///     A TX_HANDLE to the interaction object that should be released. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The object was successfully released. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txReleaseObject(
  var hObject: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Formats an interaction object as text. (primarily for debugging purposes)
/// </summary>
/// <param name="hObject">
///   <para>
///     A TX_HANDLE to an object. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="pText">
///   A TX_STRING to which the formatted text will be copied. <br />Must be at
///   least the size of the formatted text. <br />Can be NULL to only get the
///   size of the formatted text.
/// </param>
/// <param name="TextSize">
///   A TX_SIZE which will be set the size of the formatted text. <br />Must
///   not be NULL. <br />The value must be 0 if pText is NULL. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The formatted text or required size of the string was
///   successfully retreived. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT:
///   An invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of the pText was to small.
///   (*pTextSize will be set to the required size.)
/// </returns>
function txFormatObjectAsText(
  hObject: TX_CONSTHANDLE;
  pText: TX_STRING;
  var TextSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


// EyeXAsyncData.h

/// <summary>
///   Gets the result code contained by an async data. <br />Not all async data
///   objects have a result code. See the specific asynchronous call for
///   details. <br />
/// </summary>
/// <param name="hAsyncData">
///   <para>
///     A TX_CONSTHANDLE to the async data object. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Result">
///   <para>
///     A TX_RESULT which will be set to the result code.
///   </para>
///   <para>
///     Must not be NULL <br />
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The result code of the async data was successfully
///   retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX client
///   environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT: An
///   invalid argument was passed to the function. <br />TX_RESULT_NOTFOUND:
///   The async data does not have a result code.
/// </returns>
function txGetAsyncDataResultCode(
  hAsyncData: TX_CONSTHANDLE;
  var Result: TX_RESULT
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Gets the content of an asynchronous data object. <br />The content may be
///   any interaction object or nothing depending on the operation.
/// </summary>
/// <param name="hAsyncData">
///   <para>
///     A TX_CONSTHANDLE to the async data object. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hObject">
///   A TX_HANDLE will be set to the content of the async data. <br />This
///   handle must be released using txReleaseObject to avoid leaks. <br />Must
///   not be NULL. <br />The value of the pointer must be set to
///   TX_EMPTY_HANDLE. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The content of the async data was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: The async data does not
///   have any content.
/// </returns>
function txGetAsyncDataContent(
  hAsyncData: TX_CONSTHANDLE;
  var hObject: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


 // EyeXSnapshot.h

/// <summary>
///   Creates a snapshot. <br />A snapshot is used to provide the current state
///   of interactors for a specfic region of the screen to the client. <br />
///   This function can also be used to create a snapshot containing global
///   interactors, but it is recommended to <br />use
///   txCreateGlobalInteractorSnapshot for this. <br />
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context on which to create the snapshot. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hSnapshot">
///   A pointer to a TX_HANDLE which will be set to the newly created snapshot.
///   <br />This handle must be released using txReleaseObject to avoid leaks. <br />
///   Must not be NULL. <br />The value of the pointer must be set to
///   TX_EMPTY_HANDLE.
/// </param>
/// <returns>
///   TX_RESULT_OK: The snapshot was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txCreateSnapshot(
  hContext: TX_CONTEXTHANDLE;
  var hSnapshot: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Creates a snapshot with the same bounds as the supplied query. <br />This
///   is a specialization of txCreateSnapshot. Normally, when a snapshot is
///   comitted as a response to a query, it is sufficient to create a snapshot
///   with the same bounds as the query instead of calculating the bounds based
///   on the interactors.
/// </summary>
/// <param name="hQuery">
///   <para>
///     A TX_CONSTHANDLE to the query this snapshot relates to. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hSnapshot">
///   <para>
///     A pointer to a TX_HANDLE which will be set to the newly created
///     snapshot. This handle must be released using txReleaseObject to avoid
///     leaks. <br />
///   </para>
///   <para>
///     The value of the pointer must be set to TX_EMPTY_HANDLE. <br />Must
///     not be NULL. <br />
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The snapshot was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txCreateSnapshotWithQueryBounds(
  hQuery: TX_CONSTHANDLE;
  var hSnapshot: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Creates a snapshot with the same bounds and window ids as the supplied
///   query. <br />This is a specialization of txCreateSnapshot that makes it
///   easier to quickly create a typical snapshot for a query. <br />
/// </summary>
/// <param name="hQuery">
///   <para>
///     A TX_CONSTHANDLE to the query this snapshot relates to. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hSnapshot">
///   <para>
///     A pointer to a TX_HANDLE which will be set to the newly created
///     snapshot. This handle must be released using txReleaseObject to avoid
///     leaks.
///   </para>
///   <para>
///     The value of the pointer must be set to TX_EMPTY_HANDLE. Must not be
///     NULL. <br />
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The snapshot was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txCreateSnapshotForQuery(
  hQuery: TX_CONSTHANDLE;
  var hSnapshot: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Commits a snapshot asynchronously. <br />The snapshot will be sent to the
///   client. <br />
/// </summary>
/// <param name="hSnapshot">
///   A TX_HANDLE to the snapshot that should be committed. <br />Must not be
///   TX_EMPTY_HANDLE.
/// </param>
/// <param name="completionHandler">
///   A TX_ASYNCDATACALLBACK to the function that will handle the snapshot
///   result. <br /><br />The async data object provided by the
///   TX_ASYNCDATACALLBACK will contain a result code which can be retrieved
///   using txGetAsyncDataResult(). The result code will be one of the
///   follwing: <br />TX_RESULT_OK: <br />The snapshot was succesfully commited
///   to the client. <br /><br />TX_RESULT_INVALIDSNAPSHOT: <br />The snapshot
///   was rejected by the client. <br /><br />TX_RESULT_CANCELLED: <br />The
///   asynchronous operation was cancelled. <br />
/// </param>
/// <param name="userParam">
///   <para>
///     A TX_USERPARAM which will be provided as a parameter to the
///     completion callback. <br />
///   </para>
///   <para>
///     Can be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The snapshot was successfully commited. The actual result
///   of the snapshot will be provided to the callback. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txCommitSnapshotAsync(
  hSnapshot: TX_HANDLE;
  completionHandler: TX_ASYNCDATACALLBACK;
  userParam: TX_USERPARAM
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the bounds of a snapshot. <br />If the snapshot does not have any
///   bounds this call will fail. <br />
/// </summary>
/// <param name="hSnapshot">
///   <para>
///     A TX_CONSTHANDLE to the snapshot for which the bounds should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hBounds">
///   <para>
///     A TX_HANDLE which will be set to the bounds of the snapshot. This
///     handle must be released using txReleaseObject to avoid leaks. <br />
///   </para>
///   <para>
///     The value of the pointer must be set to TX_EMPTY_HANDLE. <br />Must
///     not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The bounds was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: The snapshot does not
///   have any bounds.
/// </returns>
function txGetSnapshotBounds(
  hSnapshot: TX_CONSTHANDLE;
  var hBounds: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the number of window ids held by a snapshot.
/// </summary>
/// <param name="hSnapshot">
///   <para>
///     A TX_CONSTHANDLE to the snapshot for which the number of window ids
///     should be retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="WindowIdsCount">
///   <para>
///     A pointer to a TX_SIZE which will be set to the number of window ids.
///     <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The number of window ids was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txGetSnapshotWindowIdCount(
  hSnapshot: TX_CONSTHANDLE;
  var WindowIdsCount: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets one of the window ids held by a snapshot. Which one is specified by
///   an index.
/// </summary>
/// <param name="hSnapshot">
///   <para>
///     A TX_CONSTHANDLE to the snapshot for which the window id should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="windowIdIndex">
///   <para>
///     The index of the window id to get. <br />
///   </para>
///   <para>
///     Must be a positive integer.
///   </para>
/// </param>
/// <param name="pWindowId">
///   A TX_STRING to which the window id will be copied. <br />Must be at least
///   the size of the window id. <br />Can be NULL to only get the size of the
///   window id.
/// </param>
/// <param name="WindowIdSize">
///   <para>
///     A pointer to a TX_SIZE which tells the size of pWindowId. <br />Will
///     be set to the size of the window id. <br />
///   </para>
///   <para>
///     Must not be NULL. <br />The value must be 0 if pWindowId is NULL. <br />
///   </para>
/// </param>
/// <returns>
///   <para>
///     TX_RESULT_OK: The window id or the required size of the string was
///     successfully retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///     client environment is not initialized. <br />
///     TX_RESULT_INVALIDARGUMENT: An invalid argument was passed to the
///     function. <br />TX_RESULT_INVALIDBUFFERSIZE: The size of windowId is
///     invalid (pWindowIdSize will be set to the required size).
///   </para>
///   <para>
///     TX_RESULT_NOTFOUND: The specified index was out of range.
///   </para>
/// </returns>
function txGetSnapshotWindowId(
  hSnapshot: TX_CONSTHANDLE;
  windowIdIndex: TX_INTEGER;
  pWindowId: TX_STRING;
  var WindowIdSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Adds a window id to a snapshot. <br />If a specific window id has already
///   been added this call will be ignored.
/// </summary>
/// <param name="hSnapshot">
///   <para>
///     A TX_HANDLE to the snapshot to which the window id should be added. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="windowId">
///   <para>
///     The window id as a string (window id corresponds to the windows
///     handle on Windows). <br />
///   </para>
///   <para>
///     Must not be NULL or empty string.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The window id was successfully added. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txAddSnapshotWindowId(
  hSnapshot: TX_HANDLE;
  windowId: TX_CONSTSTRING
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Creates an interactor in a snapshot. <br />If an interactor with the same
///   id has already been created this call will fail. <br />The interactor
///   will be owned by the snapshot and does not need to be removed explicitly.
/// </summary>
/// <param name="hSnapshot">
///   <para>
///     A TX_HANDLE to the snapshot in which the interactor should be
///     created. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hInteractor">
///   <para>
///     A pointer to a TX_HANDLE which will be set to the newly created
///     interactor. <br />This handle must be released using txReleaseObject
///     to avoid leaks. <br />
///   </para>
///   <para>
///     The value of the pointer must be set to TX_EMPTY_HANDLE. <br />Must
///     not be NULL.
///   </para>
/// </param>
/// <param name="interactorId">
///   <para>
///     The interactor id as a TX_CONSTSTRING. <br />Whenever some
///     interaction happens to an interactor this specifies on which
///     interactor the interaction occurred. <br />
///   </para>
///   <para>
///     Must not be NULL or empty string.
///   </para>
/// </param>
/// <param name="parentId">
///   <para>
///     The parent interactor id as a TX_CONSTSTRING. <br />If this
///     interactor does not have an explicit parent the id should be set to
///     TX_LITERAL_ROOTID.
///   </para>
///   <para>
///     Commiting a snapshot which contains orphan interactors will fail. <br />
///   </para>
///   <para>
///     Must not be NULL or empty string.
///   </para>
/// </param>
/// <param name="windowId">
///   <para>
///     The window id as a TX_CONSTSTRING. <br />Sets the top level window id
///     of an interactor (window id corresponds to the windows handle on
///     Windows). Each interactor needs to specify the top level window id in
///     which it was found. Should be set to
///     TX_LITERAL_GLOBALINTERACTORWINDOWID if this is a global interactor. <br />
///   </para>
///   <para>
///     Must not be NULL or empty string.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The interactor was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_DUPLICATEINTERACTOR: An
///   interactor with the same id already exists in this snapshot.
/// </returns>
function txCreateInteractor(
  hSnapshot: TX_HANDLE;
  var hInteractor: TX_HANDLE;
  interactorId: TX_CONSTSTRING;
  parentId: TX_CONSTSTRING;
  windowId: TX_CONSTSTRING
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Creates a rectangular interactor in a snapshot, with all required
///   parameters. <br />If an interactor with the same id has already been
///   created this call will fail. <br />The interactor will be owned by the
///   snapshot and does not need to be removed explicitly. <br />
/// </summary>
/// <param name="hSnapshot">
///   <para>
///     A TX_HANDLE to the snapshot in which the interactor should be
///     created. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hInteractor">
///   <para>
///     A TX_HANDLE which will be set to the newly created interactor. This
///     handle must be released using txReleaseObject to avoid leaks. <br />
///   </para>
///   <para>
///     The value of the pointer must be set to TX_EMPTY_HANDLE. <br />Must
///     not be NULL.
///   </para>
/// </param>
/// <param name="interactorId">
///   The interactor id as a TX_CONSTSTRING. Whenever some interaction happens
///   to an interactor this specifies on which interactor the interaction
///   occurred. <br />Must not be NULL or empty string. <br />
/// </param>
/// <param name="Bounds">
///   <para>
///     The rectangular dimensions of the interactor.
///   </para>
///   <para>
///     Must not be NULL or empty string.
///   </para>
/// </param>
/// <param name="parentId">
///   <para>
///     The parent interactor id as a TX_CONSTSTRING. <br />If this
///     interactor does not have an explicit parent the id should be set to
///     TX_LITERAL_ROOTID. Commiting a snapshot which contains orphan
///     interactors will fail. <br />
///   </para>
///   <para>
///     Must not be NULL or empty string. <br />
///   </para>
/// </param>
/// <param name="windowId">
///   <para>
///     The window id as a TX_CONSTSTRING. <br />Sets the top level window id
///     of an interactor (window id corresponds to the windows handle on
///     Windows). Each interactor needs to specify the top level window id in
///     which it was found. Should be set to
///     TX_LITERAL_GLOBALINTERACTORWINDOWID if this is a global interactor.
///   </para>
///   <para>
///     <br />Must not be NULL or empty string. <br />
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The interactor was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_DUPLICATEINTERACTOR: An
///   interactor with the same id already exists in this snapshot.
/// </returns>
function txCreateRectangularInteractor(
  hSnapshot: TX_HANDLE;
  var hInteractor: TX_HANDLE;
  interactorId: TX_CONSTSTRING;
  var Bounds: TX_RECT;
  parentId: TX_CONSTSTRING;
  windowId: TX_CONSTSTRING
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Removes an interactor from a snapshot. <br />If an interactor with the
///   specified id does not exist this call will fail. <br />The interactor is
///   owned by the snapshot and does not need to be removed explicitly.
/// </summary>
/// <param name="hSnapshot">
///   <para>
///     A TX_HANDLE to the snapshot from which the interactor should be
///     removed. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="interactorId">
///   <para>
///     The id of the interactor to remove. <br />
///   </para>
///   <para>
///     Must not be NULL or empty string.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The interactor was successfully removed. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: An interactor with the
///   specified id does not exists in the snapshot.
/// </returns>
function txRemoveInteractor(
  hSnapshot: TX_HANDLE;
  interactorId: TX_CONSTSTRING
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets TX_HANDLEs to all interactors in a snapshot. <br />
/// </summary>
/// <param name="hSnapshot">
///   <para>
///     A TX_CONSTHANDLE to the snapshot from which to get the interactors. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="phInteractors">
///   <para>
///     A pointer to an array of TX_HANDLEs to which the interactor handles
///     will be copied. These handles must be released using txReleaseObject
///     to avoid leaks. <br />
///   </para>
///   <para>
///     Can be NIL but to only get the size.
///   </para>
/// </param>
/// <param name="InteractorsSize">
///   <para>
///     A pointer to a TX_SIZE which will be set to the number of
///     interactors. <br />Must not be NULL.
///   </para>
///   <para>
///     <br />The value must be 0 if phInteractors is NIL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The handles or the required size of the buffer were
///   retrieved successfully. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT:
///   An invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of the array is invalid.
///   (*pInteractorsSize will be set to the number of interactors).
/// </returns>
function txGetInteractors(
  hSnapshot: TX_CONSTHANDLE;
  phInteractors: PTXHANDLE;
  var InteractorsSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   <para>
///     Creates bounds on a snapshot. <br />The bounds of a snapshot should
///     specify a rectangle that defines the region of the screen for which
///     interactors are provided. Typically these are the same bounds as on
///     the query. The bounds may cover a larger area, thus telling the
///     client where there is empty space. If the bounds does not at least
///     intersect the interactors provided in the snapshot
///     txCommitSnapshotAsync will fail. <br />
///   </para>
///   <para>
///     If the snapshot already have bounds this call will fail.
///   </para>
/// </summary>
/// <param name="hSnapshot">
///   <para>
///     A TX_HANDLE to the snapshot on which the bounds should be created.
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE. <br />
///   </para>
/// </param>
/// <param name="hBounds">
///   <para>
///     A TX_HANDLE which will be set to the newly created bounds. <br />This
///     handle must be released using txReleaseObject to avoid leaks. <br />
///   </para>
///   <para>
///     The value of the pointer must be set to TX_EMPTY_HANDLE. <br />Must
///     not be NULL. <br />
///   </para>
/// </param>
/// <param name="boundsType">
///   A TX_BOUNDSTYPE which specifies the type of bounds to create.
/// </param>
/// <returns>
///   TX_RESULT_OK: The bounds was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_DUPLICATEBOUNDS: The snapshot
///   already has bounds.
/// </returns>
function txCreateSnapshotBounds(
  hSnapshot: TX_HANDLE;
  var hBounds: TX_HANDLE;
  boundsType: TX_BOUNDSTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Deletes the bounds on a snapshot. <br />If the snapshot does not have any
///   bounds this call will fail.
/// </summary>
/// <param name="hSnapshot">
///   <para>
///     A TX_HANDLE to the snapshot on which the bounds should be deleted. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The bounds was successfully deleted. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: The snapshot does not
///   have any bounds.
/// </returns>
function txDeleteSnapshotBounds(
  hSnapshot: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   <para>
///     Creates a global Interactor Snapshot.
///   </para>
///   <para>
///     Creates a snapshot with: <br />Bounds with boundsType
///     TX_BOUNDSTYPE_NONE, <br />windowId as
///     TX_LITERAL_GLOBALINTERACTORWINDOWID, <br />One interactor with: <br />
///     Bounds with boundsType TX_BOUNDSTYPE_NONE, <br />ParentId as
///     TX_LITERAL_ROOTID <br />WindowId as
///     TX_LITERAL_GLOBALINTERACTORWINDOWID, <br />InteractorId as
///     @interactorId. <br />
///   </para>
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context on which to create the snapshot. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="interactorId">
///   <para>
///     The Id of the interactor that will be added to the snapshot. <br />
///   </para>
///   <para>
///     Must not be the empty string.
///   </para>
/// </param>
/// <param name="hSnapshot">
///   A handle of the created snapshot. <br />
/// </param>
/// <param name="hInteractor">
///   A handle of the created interactor object. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The interactor was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txCreateGlobalInteractorSnapshot(
  hContext: TX_CONTEXTHANDLE;
  interactorId: TX_CONSTSTRING;
  var hSnapshot: TX_HANDLE;
  var hInteractor: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


//* EyeXBounds.h

/// <summary>
///   Gets the TX_BOUNDSTYPE of an interaction bounds object.
/// </summary>
/// <param name="hBounds">
///   <para>
///     A TX_CONSTHANDLE to the bounds. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="BoundsType">
///   <para>
///     A pointer to a TX_BOUNDSTYPE which will be set to the type of the
///     bounds. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The type of the bounds was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txGetBoundsType(
  hBounds: TX_CONSTHANDLE;
  var BoundsType: TX_BOUNDSTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Sets rectangular bounds data for a bounds object. <br />
/// </summary>
/// <param name="hBounds">
///   <para>
///     A TX_HANDLE to the bounds on which to set the rectangle. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="x">
///   Position of left edge of the rectangle.
/// </param>
/// <param name="y">
///   Position of top edge of the rectangle. <br />
/// </param>
/// <param name="width">
///   Width of the rectangle. Must not be negative.
/// </param>
/// <param name="height">
///   Height of the rectangle. Must not be negative.
/// </param>
/// <returns>
///   TX_RESULT_OK: The rectangular data was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBOUNDSTYPE: The bounds
///   type was invalid, must be TX_BOUNDSTYPE_RECTANGULAR.
/// </returns>
function txSetRectangularBoundsData(
  hBounds: TX_HANDLE;
  x: TX_REAL;
  y: TX_REAL;
  width: TX_REAL;
  height: TX_REAL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets rectangular bounds data for a bounds object.
/// </summary>
/// <param name="hBounds">
///   <para>
///     A TX_HANDLE to the bounds object on which to set the rectangle. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Rect">
///   <para>
///     A pointer to a TX_RECT which holds the rectangular data. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The rectangular data was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBOUNDSTYPE: The bounds
///   type was invalid, must be TX_BOUNDSTYPE_RECTANGULAR.
/// </returns>
function txSetRectangularBoundsDataRect(
  hBounds: TX_HANDLE;
  var Rect: TX_RECT
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets rectangular bounds data from a bounds object. <br />
/// </summary>
/// <param name="hBounds">
///   <para>
///     A TX_CONSTHANDLE to the bounds object from which to get the
///     rectangular data. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="X">
///   <para>
///     A TX_REAL which will be set to the position of the left edge of the
///     rectangle. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <param name="Y">
///   A TX_REAL which will be set to the position of the top edge of the
///   rectangle. <br /><br />Must not be NULL.
/// </param>
/// <param name="Width">
///   <para>
///     A TX_REAL which will be set to the width of the rectangle. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <param name="Height">
///   <para>
///     A pointer to a TX_REAL which will be set to the height of the
///     rectangle.
///   </para>
///   <para>
///     <br />Must not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The rectangular data was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBOUNDSTYPE: The bounds
///   type is invalid, must be TX_BOUNDSTYPE_RECTANGULAR.
/// </returns>
function txGetRectangularBoundsData(
  hBounds: TX_CONSTHANDLE;
  var X: TX_REAL;
  var Y: TX_REAL;
  var Width: TX_REAL;
  var Height: TX_REAL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets rectangular bounds data from a bounds object.
/// </summary>
/// <param name="hBounds">
///   <para>
///     A TX_CONSTHANDLE to the Bounds on which to get the rectangle data.
///   </para>
///   <para>
///     <br />Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Rect">
///   <para>
///     A TX_RECT which will hold the rectangle data. <br />
///   </para>
///   <para>
///     Must not be NULL. <br />
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The rectangular data was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBOUNDSTYPE: The bounds
///   type is invalid, must be TX_BOUNDSTYPE_RECTANGULAR.
/// </returns>
function txGetRectangularBoundsDataRect(
  hBounds: TX_CONSTHANDLE;
  var Rect: TX_RECT
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Checks if a bound intersects with a rectangle.
/// </summary>
/// <param name="hBounds">
///   The bounds to check intersection with.
/// </param>
/// <param name="x2">
///   The upper left x coordinate of the rectangle
/// </param>
/// <param name="y2">
///   The upper left y coordinate of the rectangle
/// </param>
/// <param name="width2">
///   The width of the rectangle
/// </param>
/// <param name="height2">
///   The height of the rectangle
/// </param>
/// <param name="Intersects">
///   The intersection test result. Will be non-zero if rectangles intersects. <br />
///   Must not be NULL.
/// </param>
function txBoundsIntersect(
  hBounds: TX_CONSTHANDLE;
  x2: TX_REAL;
  y2: TX_REAL;
  width2: TX_REAL;
  height2: TX_REAL;
  var Intersects: TX_BOOL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}



/// <summary>
///   Checks if a bound intersects with a rectangle.
/// </summary>
/// <param name="hBounds">
///   The bounds to check intersection with.
/// </param>
/// <param name="Rect2">
///   The rectangle to check intersection with.
/// </param>
/// <param name="Intersects">
///   The intersection test result. Will be non-zero if rectangles intersects.
/// </param>
function txBoundsIntersectRect(
  hBounds: TX_CONSTHANDLE;
  var Rect2: TX_RECT;
  var Intersects: TX_BOOL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


// EyeXInteractor.h

/// <summary>
///   Gets the id of an interactor. <br />
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_CONSTHANDLE to the interactor for which the id should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="pInteractorId">
///   A TX_STRING to which the interactor id will be copied. <br />Must be at
///   least the size of the interactor id. <br /><br />Can be NULL to only get
///   the size of the interactor id.
/// </param>
/// <param name="InteractorIdSize">
///   <para>
///     A pointer to a TX_SIZE which will be set the size of the interactor
///     id. <br />The value must be 0 if pInteractorId is NULL. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The interactor id or the required size of the string was
///   successfully retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT:
///   An invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of pInteractorId is invalid
///   (*pInteractorIdSize will be set to the required size).
/// </returns>
function txGetInteractorId(
  hInteractor: TX_CONSTHANDLE;
  pInteractorId: TX_STRING;
  var InteractorIdSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the id of an interactors parent. <br />
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_CONSTHANDLE to the interactor for which the parent id should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="pParentInteractorId">
///   <para>
///     A TX_STRING to which the parent interactor id will be copied. Must be
///     at least the size of the parent interactor id. <br />
///   </para>
///   <para>
///     Can be NULL to only get the size of the parent interactor id. <br />
///   </para>
/// </param>
/// <param name="InteractorParentIdSize">
///   <para>
///     A pointer to a TX_SIZE which will be set the size of the parent
///     interactor id. <br />
///   </para>
///   <para>
///     The value must be 0 if pParentInteractorId is NULL.
///   </para>
///   <para>
///     Must not be NULL. <br />
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The parent interactor id or the required size of the string
///   was successfully retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT:
///   An invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of pParentInteractorId is invalid
///   (*pInteractorParentIdSize will be set to the required size). <br />
/// </returns>
function txGetInteractorParentId(
  hInteractor: TX_CONSTHANDLE;
  pParentInteractorId: TX_STRING;
  var InteractorParentIdSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the window id of the interactor.
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_CONSTHANDLE to the interactor for which the window id should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="pWindowId">
///   <para>
///     A TX_STRING to which the window id will be copied. Must be at least
///     the size of the window id. <br />
///   </para>
///   <para>
///     Can be NULL to only get the size of the window id. <br />
///   </para>
/// </param>
/// <param name="WindowIdSize">
///   <para>
///     A pointer to a TX_SIZE which will be set the size of the window id. <br />
///   </para>
///   <para>
///     The value must be 0 if pWindowId is NULL. <br />Must not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The window id or the required size of the string was
///   successfully retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT:
///   An invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of pWindowId is invalid
///   (*pWindowIdSize will be set to the required size). <br />
/// </returns>
function txGetInteractorWindowId(
  hInteractor: TX_CONSTHANDLE;
  pWindowId: TX_STRING;
  var WindowIdSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   <para>
///     Sets the z value of an interactor. <br />
///   </para>
///   <para>
///     The z value of an interactor is used to specify which interactor lies
///     on top of which among siblings. <br />Note that the z value is local
///     on each level of interactors and therefore only concerns interactors
///     with the same parent. <br />
///   </para>
///   <para>
///     The z value must be a non negative TX_REAL. <br />
///   </para>
///   <para>
///     This call will overwrite any previously set z value.
///   </para>
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_HANDLE to the interactor for which to set the z value. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="z">
///   <para>
///     The z value as a TX_REAL. <br />
///   </para>
///   <para>
///     Must be a non negative TX_REAL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The z value was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetInteractorZ(
  hInteractor: TX_HANDLE;
  z: TX_REAL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the z value of an interactor.
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_CONSTHANDLE to the interactor for which to get the z value. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Z">
///   <para>
///     The pointer to a TX_REAL which will be set to the z value. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The z value was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txGetInteractorZ(
  hInteractor: TX_CONSTHANDLE;
  var Z: TX_REAL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   <para>
///     Sets a TX_BOOL flag which specifies if an interactor is enabled or
///     not. <br />
///   </para>
///   <para>
///     This call will overwrite any previously set enabled flag. <br />The
///     default value is false. <br />
///   </para>
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_HANDLE to the interactor to enable/disable. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="enabled">
///   The flag as a TX_BOOL.
/// </param>
/// <returns>
///   TX_RESULT_OK: The interactor was successfully set to enabled/disabled. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetInteractorEnabled(
  hInteractor: TX_HANDLE;
  enabled: TX_BOOL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the enabled flag from the interactor. <br />
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_CONSTHANDLE to the interactor for which to get the enabled flag.
///     <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Enabled">
///   <para>
///     The pointer to a TX_BOOL which will be set to the enabled flag value.
///     <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The enabled flag was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txGetInteractorEnabled(
  hInteractor: TX_CONSTHANDLE;
  var Enabled: TX_BOOL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   <para>
///     Sets a TX_BOOL flag which specifies if an interactor is deleted or
///     not.
///   </para>
///   <para>
///     This call will overwrite any previously set deleted flag. <br />The
///     default value is false. <br />
///   </para>
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_HANDLE to the interactor to set as deleted. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="deleted">
///   The flag as a TX_BOOL.
/// </param>
/// <returns>
///   TX_RESULT_OK: The deleted flag on the interactor was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetInteractorDeleted(
  hInteractor: TX_HANDLE;
  deleted: TX_BOOL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the deleted flag from the interactor.
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_CONSTHANDLE to the interactor for which to get the deleted flag.
///     <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Deleted">
///   <para>
///     The pointer to a TX_BOOL which will be set to the deleted flag value.
///     <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The deleted flag was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txGetInteractorDeleted(
  hInteractor: TX_CONSTHANDLE;
  var Deleted: TX_BOOL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   <para>
///     Creates bounds on an interactor.
///   </para>
///   <para>
///     If the interactor already has bounds this call will fail. <br />The
///     bounds will be owned by the interactor and does not need to be
///     deleted explicitly. <br />
///   </para>
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_HANDLE to the interactor on which the bounds should be created. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hBounds">
///   <para>
///     A TX_HANDLE which will be set to the newly created bounds. This
///     handle must be released using txReleaseObject to avoid leaks. <br />
///   </para>
///   <para>
///     Must not be NULL. <br />The value of the pointer must be set to
///     TX_EMPTY_HANDLE. <br />
///   </para>
/// </param>
/// <param name="boundsType">
///   The TX_BOUNDSTYPE which specifies what kind of bounds to create.
/// </param>
/// <returns>
///   TX_RESULT_OK: The bounds was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_DUPLICATEBOUNDS: This interactor
///   already has bounds.
/// </returns>
function txCreateInteractorBounds(
  hInteractor: TX_HANDLE;
  var hBounds: TX_HANDLE;
  boundsType: TX_BOUNDSTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   <para>
///     Deletes the bounds on an interactor. <br />
///   </para>
///   <para>
///     If the interactor does not have any bounds this call will fail. <br />
///     The bounds object is owned by the interactor and does not need to be
///     deleted explicitly. <br />
///   </para>
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_HANDLE to the interactor on which to delete the bounds. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The bounds was successfully deleted. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: This interactor does
///   not have any bounds.
/// </returns>
function txDeleteInteractorBounds(
  hInteractor: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   <para>
///     Gets the bounds of an interactor. <br />
///   </para>
///   <para>
///     If the interactor does not have any bounds this call will fail.
///   </para>
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_CONSTHANDLE to the interactor from which the bounds should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE. <br />
///   </para>
/// </param>
/// <param name="hBounds">
///   <para>
///     A TX_HANDLE which will be set to the bounds on the interactor. This
///     handle must be released using txReleaseObject to avoid leaks. <br />
///   </para>
///   <para>
///     Must not be NULL. <br />The value of the pointer must be set to
///     TX_EMPTY_HANDLE. <br />
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The bounds was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: This interactor does
///   not have any bounds.
/// </returns>
function txGetInteractorBounds(
  hInteractor: TX_CONSTHANDLE;
  var hBounds: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   <para>
///     Creates a behavior of a specified TX_BEHAVIORTYPE on an interactor. <br />
///   </para>
///   <para>
///     If the interactor already has a behavior of the specified type this
///     call will fail. <br />The behavior will be owned by the interactor
///     and does not need to be removed explicitly. <br />
///   </para>
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_HANDLE to the interactor on which the behavior should be
///     created. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hBehavior">
///   <para>
///     A TX_HANDLE which will be set to the newly created behavior.
///   </para>
///   <para>
///     This handle must be released using txReleaseObject to avoid leaks. <br />
///   </para>
///   <para>
///     Must not be NULL. <br />
///   </para>
///   <para>
///     The value of the TX_HANDLE must be set to TX_EMPTY_HANDLE. <br />
///   </para>
/// </param>
/// <param name="behaviorType">
///   The TX_BEHAVIORTYPE which specifies what type of behavior to create.
/// </param>
/// <returns>
///   TX_RESULT_OK: The behavior was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_DUPLICATEBEHAVIOR: This
///   interactor already has a behavior of the specified type.
/// </returns>
function txCreateInteractorBehavior(
  hInteractor: TX_HANDLE;
  var hBehavior: TX_HANDLE;
  behaviorType: TX_BEHAVIORTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   <para>
///     Removes a TX_BEHAVIORTYPE from an interactor. <br />
///   </para>
///   <para>
///     If the interactor does not have a behavior of the specified type this
///     call will fail. <br />
///   </para>
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_HANDLE to the interactor from which the behavior should be
///     removed. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="behaviorType">
///   The TX_BEHAVIORTYPE which specifies what type of behavior to remove.
/// </param>
/// <returns>
///   TX_RESULT_OK: The behavior was successfully removed. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: This interactor does
///   not have a behavior of the specified type. <br />
/// </returns>
function txRemoveInteractorBehavior(
  hInteractor: TX_HANDLE;
  behaviorType: TX_BEHAVIORTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   <para>
///     Gets a TX_BEHAVIORTYPE from an interactor. <br />
///   </para>
///   <para>
///     If the interactor does not have a behavior of the specified type this
///     call will fail.
///   </para>
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_CONSTHANDLE to the interactor from which the behavior should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hBehavior">
///   <para>
///     A TX_HANDLE which will be set to the behavior. <br />This handle must
///     be released using txReleaseObject to avoid leaks. <br />
///   </para>
///   <para>
///     The value of the pointer must be set to TX_EMPTY_HANDLE. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <param name="behaviorType">
///   The TX_BEHAVIORTYPE which specifies what type of behavior to get.
/// </param>
/// <returns>
///   TX_RESULT_OK: The behavior was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: This interactor does
///   not have a behavior of the specified type.
/// </returns>
function txGetInteractorBehavior(
  hInteractor: TX_CONSTHANDLE;
  var hBehavior: TX_HANDLE;
  behaviorType: TX_BEHAVIORTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


(**

  @param hInteractor [in]:

  @param phBehaviors [out]:

  @param pBehaviorsSize [in,out]:

  @return
 *)
/// <summary>
///   Gets the TX_HANDLEs to all behaviors on an interactor.
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_CONSTHANDLE to the interactor from which to get the behaviors. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="phBehaviors">
///   <para>
///     A pointer to an array of TX_HANDLEs to which the behavior handles
///     will be copied. <br />
///   </para>
///   <para>
///     These handles must be released using txReleaseObject to avoid leaks.
///   </para>
///   <para>
///     Can be nil to only get the size.
///   </para>
/// </param>
/// <param name="BehaviorsSize">
///   <para>
///     A TX_SIZE which will be set to the number of behaviors. <br />
///   </para>
///   <para>
///     The value must be 0 if phBehaviors is nil. <br />
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The handles or the required size of the buffer was
///   retrieved successfully. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT:
///   An invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of the array is invalid.
///   (*pBehaviorsSize will be set to the number of behaviors). <br />
/// </returns>
function txGetInteractorBehaviors(
  hInteractor: TX_CONSTHANDLE;
  phBehaviors: PTXHANDLE;
  var BehaviorsSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Creates a gaze aware behavior and attaches it to the interactor.
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_HANDLE to the interactor that should have the behavior. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Params">
///   A TX_GAZEAWAREARAMS which specifies the behaviors parameters. <br />Must
///   not be NULL. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The behavior was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_DUPLICATEBEHAVIOR: This
///   interactor already has a behavior of the specified type.
/// </returns>
function txCreateGazeAwareBehavior(
  hInteractor: TX_HANDLE;
  var Params: TX_GAZEAWAREPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Creates an activatable behavior and attaches it to the interactor.
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_HANDLE to the interactor that should have the behavior. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Params">
///   <para>
///     A TX_ACTIVATABLEPARAMS which specifies the behaviors
///     parameters. <br />
///   </para>
///   <para>
///     Must not be NULL. <br />
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The behavior was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_DUPLICATEBEHAVIOR: This
///   interactor already has a behavior of the specified type.
/// </returns>
function txCreateActivatableBehavior(
  hInteractor: TX_HANDLE;
  var Params: TX_ACTIVATABLEPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Creates a pannable behavior and attaches it to the interactor.
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_HANDLE to the interactor that should have the behavior. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Params">
///   <para>
///     A pointer to a TX_PANNABLEPARAMS which specifies the behaviors
///     parameters. <br />
///   </para>
///   <para>
///     Must not be NULL. <br />
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The behavior was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_DUPLICATEBEHAVIOR: This
///   interactor already has a behavior of the specified type.
/// </returns>
function txCreatePannableBehavior(
  hInteractor: TX_HANDLE;
  var Params: TX_PANNABLEPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Creates a gaze point data behavior and attaches it to the interactor. <br />
/// </summary>
/// <param name="hInteractor">
///   A TX_HANDLE to the interactor that should have the behavior. <br />Must
///   not be TX_EMPTY_HANDLE. <br />
/// </param>
/// <param name="Params">
///   A TX_GAZEPOINTDATAPARAMS which specifies the behaviors parameters. Must
///   not be NULL. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The behavior was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_DUPLICATEBEHAVIOR: This
///   interactor already has a behavior of the specified type.
/// </returns>

function txCreateGazePointDataBehavior(
  hInteractor: TX_HANDLE;
  var Params: TX_GAZEPOINTDATAPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Creates a fixation data behavior and attaches it to the interactor.
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_HANDLE to the interactor that should have the behavior. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Params">
///   <para>
///     A TX_FIXATIONDATAPARAMS which specifies the behaviors parameters. <br />
///   </para>
///   <para>
///     Must not be NULL. <br />
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The behavior was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_DUPLICATEBEHAVIOR: This
///   interactor already has a behavior of the specified type.
/// </returns>
function txCreateFixationDataBehavior(
  hInteractor: TX_HANDLE;
  var Params: TX_FIXATIONDATAPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   <para>
///     Creates an mask on an interactor.
///   </para>
///   <para>
///     The mask is defined by a matrix of size columnCount * rowCount. The
///     usage of each element in the matrix varies between diffrent
///     TX_MASKTYPEs. A mask should typically NOT correspond to the number of
///     pixels covering an interactor, rather it should be smaller. <br />
///   </para>
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_HANDLE to interactor on which to create the mask.
///   </para>
///   <para>
///     <br />Must not be TX_EMPTY_HANDLE. <br />
///   </para>
/// </param>
/// <param name="hMask">
///   <para>
///     A TX_HANDLE which will be set to the newly created mask. <br />
///   </para>
///   <para>
///     Must not be NULL. The value of the pointer must be set to
///     TX_EMPTY_HANDLE. <br />
///   </para>
/// </param>
/// <param name="maskType">
///   The type of mask to create. See TX_MASKTYPE.
/// </param>
/// <param name="columnCount">
///   The width of the mask. <br />Must be a positive TX_INTEGER. <br />
///   columnCount*rowCount must not be larger than 65536. <br />
/// </param>
/// <param name="rowCount">
///   The height of the mask. <br />Must be a positive TX_INTEGER. <br />
///   columnCount*rowCount must not be larger than 65536.
/// </param>
/// <param name="pData">
///   A pointer to buffer of TX_BYTEs which contains the mask elements. <br />
///   See TX_MASKTYPE for details about mask data for different types. <br />
///   Must not be NULL. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The mask was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_MASKTOOLARGE: The mask is too
///   large, columnCount*rowCount must not be larger than 65536 (e.g. 256*256,
///   128*512 etc). <br />
/// </returns>
function txCreateMask(
  hInteractor: TX_HANDLE;
  var hMask: TX_HANDLE;
  maskType: TX_MASKTYPE;
  columnCount: TX_INTEGER;
  rowCount: TX_INTEGER;
  pData: PTXBYTE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Removes an mask from an interactor.
/// </summary>
/// <param name="hInteractor">
///   A TX_HANDLE to interactor on which to create the mask. <br />Must not be
///   TX_EMPTY_HANDLE.
/// </param>
/// <returns>
///   TX_RESULT_OK: The mask was successfully removed. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txRemoveMask(
  hInteractor: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Retrieves the mask from an interactor.
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_CONSTHANDLE to interactor from which to get the mask.
///   </para>
///   <para>
///     <br />Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hMask">
///   <para>
///     A TX_HANDLE which will be set to the mask. <br />
///   </para>
///   <para>
///     The value of the pointer must be set to TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The mask was successfully removed. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: The interactor does not
///   have an mask.
/// </returns>
function txGetMask(
  hInteractor: TX_CONSTHANDLE;
  var hMask: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the data of an mask.
/// </summary>
/// <param name="hMask">
///   <para>
///     A TX_CONSTHANDLE to mask for which to get the data. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="ColumnCount">
///   <para>
///     A TX_INTEGER which will be set to the width of the mask. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <param name="RowCount">
///   <para>
///     A TX_INTEGER which will be set to the height of the mask. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <param name="pData">
///   <para>
///     A pointer to buffer of TX_BYTEs which contains the mask elements. <br />
///   </para>
///   <para>
///     Can be NULL to only get the size of the mask.
///   </para>
/// </param>
/// <param name="DataSize">
///   <para>
///     A TX_SIZE which will be set to the size of the data buffer. <br />
///   </para>
///   <para>
///     The value must be 0 if pData is NULL. <br />
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The data of the mask or the required buffer size was
///   successfully retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDBUFFERSIZE:
///   The size of the buffer was to small. (*pDataSize will be set to the
///   required size if not NULL.) <br />TX_RESULT_INVALIDARGUMENT: An invalid
///   argument was passed to the function.
/// </returns>
function txGetMaskData(
  hMask: TX_CONSTHANDLE;
  var ColumnCount: TX_INTEGER;
  var RowCount: TX_INTEGER;
  pData: TX_BYTE;
  var DataSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Sets the bounds of the mask. <br />By default a mask covers the entire
///   interactor.
/// </summary>
/// <param name="hInteractor">
///   A TX_HANDLE to interactor for which to set the mask bounds. Must not be
///   TX_EMPTY_HANDLE.
/// </param>
/// <param name="Bounds">
///   A TX_RECT which holds the rectangle data.
/// </param>
/// <returns>
///   TX_RESULT_OK: The bounds of the mask was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetMaskBounds(
  hInteractor: TX_HANDLE;
  var Bounds: TX_RECT
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Clears the mask bounds of an interactor. <br />
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_HANDLE to interactor for which to set the mask bounds. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The bounds of the mask was successfully cleared. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txClearMaskBounds(
  hInteractor: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the bounds of the mask.
/// </summary>
/// <param name="hInteractor">
///   <para>
///     A TX_CONSTHANDLE to interactor from which to set the mask bounds. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Bounds">
///   A TX_RECT which will hold the rectangle data. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The bounds of the mask was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: The interactor does not
///   have any mask bounds specified.
/// </returns>
function txGetMaskBounds(
  hInteractor: TX_CONSTHANDLE;
  var Bounds: TX_RECT
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


// EyeXCommand.h

/// <summary>
///   Creates a command. Internal: In first hand, prefer a higher abstraction
///   before sending raw commands to the engine.
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context on which to create the command.
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hCommand">
///   <para>
///     A TX_HANDLE which will be set to the newly created command. <br />
///     This handle must be released using txReleaseObject to avoid leaks. <br />
///   </para>
///   <para>
///     The value of the pointer must be set to TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="commandType">
///   The type of the command.
/// </param>
/// <returns>
///   TX_RESULT_OK: The command was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDCONTEXT: The handle to the
///   context was invalid.
/// </returns>
function txCreateCommand(
  hContext: TX_CONTEXTHANDLE;
  var hCommand: TX_HANDLE;
  commandType: TX_COMMANDTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_COMMANDTYPE of a command.
/// </summary>
/// <param name="hCommand">
///   <para>
///     A TX_CONSTHANDLE to the command.
///   </para>
///   <para>
///     <br />Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="CommandType">
///   A TX_COMMANDTYPE which will be set to the type of the command. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The type of the command was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txGetCommandType(
  hCommand: TX_CONSTHANDLE;
  var CommandType: TX_COMMANDTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets the data of a command. <br />If the command already has some object
///   set as data it will be replaced and released.
/// </summary>
/// <param name="hCommand">
///   <para>
///     A TX_HANDLE to the command. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hObject">
///   <para>
///     A TX_HANDLE to the object that should represent the data of the
///     command. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The data of the command was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetCommandData(
  hCommand: TX_HANDLE;
  hObject: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the data of a command.
/// </summary>
/// <param name="hCommand">
///   <para>
///     A TX_CONSTHANDLE to the command. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hObject">
///   <para>
///     A TX_HANDLE to which the handle of the object used as data will be
///     copied. This handle must be released using txReleaseObject to avoid
///     leaks. <br />
///   </para>
///   <para>
///     The value of the pointer must be set to TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The data of the command was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: The command does not
///   have any data.
/// </returns>
function txGetCommandData(
  hCommand: TX_CONSTHANDLE;
  var hObject: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Executes a command asynchronously.
/// </summary>
/// <param name="hCommand">
///   A TX_HANDLE to the command. <br />Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="completionHandler">
///   The TX_ASYNCDATACALLBACK that will handle the command result. <br />Can
///   be NULL. <br />The data provided by the TX_ASYNCDATACALLBACK will contain
///   a result code which can be retrieved using txGetAsyncDataResult(). The
///   result code will be one of the following: <br /><br />TX_RESULT_OK: <br />
///   The command was successfully executed on the client. <br /><br />
///   TX_RESULT_INVALIDCOMMAND: <br />The command was rejected by the client. <br /><br />
///   TX_RESULT_CANCELLED: <br />The asynchronous operation was canceled. <br /><br />
///   That handle to the async data must NOT be released. <br />
/// </param>
/// <param name="userParam">
///   <para>
///     A TX_USERPARAM which will be provided as a parameter to the
///     completion callback. <br />
///   </para>
///   <para>
///     Can be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The command was successfully executed. The actual result of
///   the command will be provided to the callback. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txExecuteCommandAsync(
  hCommand: TX_HANDLE;
  completionHandler: TX_ASYNCDATACALLBACK;
  userParam: TX_USERPARAM
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

// EyeXAction.h

/// <summary>
///   Creates an Action command. <br />
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context on which to create the command. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hCommand">
///   <para>
///     A TX_HANDLE which will be set to the newly created command. This
///     handle must be released using txReleaseObject to avoid leaks. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="actionType">
///   The type of action.
/// </param>
/// <returns>
///   TX_RESULT_OK: The command was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txCreateActionCommand(
  hContext: TX_CONTEXTHANDLE;
  var hCommand: TX_HANDLE;
  actionType: TX_ACTIONTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Disables EyeX builtin keys for a top-level window. When the gaze is over
///   the specified window, all interaction must be done through action
///   commands. <br />
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context on which to disable keys. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="windowId">
///   The window id for which to disable keys (window id corresponds to the
///   windows handle on Windows).
/// </param>
/// <param name="completionHandler">
///   <para>
///     The TX_ASYNCDATACALLBACK that will handle the request result. <br />
///   </para>
///   <para>
///     Can be NULL.
///   </para>
///   <para>
///     The data provided by the TX_ASYNCDATACALLBACK will contain a result
///     code which can be retrieved using <br /><br />TX_RESULT_OK: <br />The
///     request was succesfully executed on the client. <br /><br />
///     TX_RESULT_CANCELLED: <br />The asynchronous operation was cancelled. <br /><br />
///     That handle to the async data must NOT be released.
///   </para>
/// </param>
/// <param name="userParam">
///   <para>
///     A TX_USERPARAM which will be provided as a parameter to the
///     completion callback. <br />
///   </para>
///   <para>
///     Can be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The request was successfully sent. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txDisableBuiltinKeys(
  hContext: TX_CONTEXTHANDLE;
  windowId: TX_CONSTSTRING;
  completionHandler: TX_ASYNCDATACALLBACK;
  userParam: TX_USERPARAM
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Enables EyeX builtin keys for a top-level window where the keys was
///   previously disabled. <br />
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context on which to enable keys. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="windowId">
///   The window id for which to re-enable keys (window id corresponds to the
///   windows handle on Windows).
/// </param>
/// <param name="completionHandler">
///   <para>
///     The TX_ASYNCDATACALLBACK that will handle the request result. <br />
///   </para>
///   <para>
///     Can be NULL. <br /><br />The data provided by the
///     TX_ASYNCDATACALLBACK will contain a result code which can be
///     retrieved using txGetAsyncDataResult(). The result code will be one
///     of the following: <br /><br />TX_RESULT_OK: <br />The request was
///     succesfully executed on the client. <br /><br />TX_RESULT_CANCELLED: <br />
///     The asynchronous operation was cancelled. <br />
///   </para>
///   <para>
///     That handle to the async data must NOT be released.
///   </para>
/// </param>
/// <param name="userParam">
///   <para>
///     A TX_USERPARAM which will be provided as a parameter to the
///     completion callback. <br />
///   </para>
///   <para>
///     Can be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The command was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txEnableBuiltinKeys(
  hContext: TX_CONTEXTHANDLE;
  windowId: TX_CONSTSTRING;
  completionHandler: TX_ASYNCDATACALLBACK;
  userParam: TX_USERPARAM
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Launch a configuration tool. The supported tools are: <br />- EyeX
///   Settings <br />- Test eye tracking <br />- Recalibrate current user
///   profile <br />- Create new user profile <br />- Guest calibration <br />-
///   Diagnostics
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="configurationTool">
///   A TX_CONFIGURATIONTOOL that determines which tool to launch.
/// </param>
/// <param name="completionHandler">
///   <para>
///     The TX_ASYNCDATACALLBACK that will handle the request result. <br />
///   </para>
///   <para>
///     Can be NULL.
///   </para>
///   <para>
///     The data provided by the TX_ASYNCDATACALLBACK will contain a result
///     code which can be retrieved using txGetAsyncDataResult(). The result
///     code will be one of the following: <br /><br />TX_RESULT_OK: <br />
///     The tool was successfully launched. <br /><br />
///     TX_RESULT_INVALIDEYETRACKERSTATE: <br />The tool can not be launched
///     in the current eye tracker state. <br />This could be that another
///     configuration tool is active or that <br />the eye tracker is in an
///     invalid state to start the configuration tool, <br />see
///     TX_CONFIGURATIONTOOL for details. <br /><br />TX_RESULT_NOTFOUND: <br />
///     The tool was not found or failed to launch. <br /><br />
///     TX_RESULT_CANCELLED: <br />The client is not connected. <br />
///   </para>
///   <para>
///     The handle to the async data must NOT be released.
///   </para>
/// </param>
/// <param name="userParam">
///   <para>
///     A TX_USERPARAM which will be provided as a parameter to the
///     completion callback. <br />
///   </para>
///   <para>
///     Can be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The command was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txLaunchConfigurationTool(
  hContext: TX_CONTEXTHANDLE;
  configurationTool: TX_CONFIGURATIONTOOL;
  completionHandler: TX_ASYNCDATACALLBACK;
  userParam: TX_USERPARAM
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Set the current calibration profile.
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="profileName">
///   The name of the profile to activate. Must be one of the available
///   profiles, see state TX_STATEPATH_EYETRACKINGPROFILES.
/// </param>
/// <param name="completionHandler">
///   The TX_ASYNCDATACALLBACK that will handle the request result. <br />Can
///   be NULL. <br />The data provided by the TX_ASYNCDATACALLBACK will contain
///   a result code which can be retrieved using <br />txGetAsyncDataResult().
///   The result code will be one of the following: <br /><br />TX_RESULT_OK: <br />
///   The profile was successfully set. <br /><br />TX_RESULT_NOTFOUND: <br />
///   The profile was not found among the available profiles. <br /><br />The
///   handle to the async data must NOT be released. <br />
/// </param>
/// <param name="userParam">
///   <para>
///     A TX_USERPARAM which will be provided as a parameter to the
///     completion callback. <br />
///   </para>
///   <para>
///     Can be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The command was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetCurrentProfile(
  hContext: TX_CONTEXTHANDLE;
  profileName: TX_CONSTSTRING;
  completionHandler: TX_ASYNCDATACALLBACK;
  userParam: TX_USERPARAM): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Delete a calibration profile.
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="profileName">
///   The name of the profile to delete. Must be one of the available profiles,
///   see state TX_STATEPATH_EYETRACKINGPROFILES.
/// </param>
/// <param name="completionHandler">
///   <para>
///     The TX_ASYNCDATACALLBACK that will handle the request result. <br />
///   </para>
///   <para>
///     Can be NULL. <br />
///   </para>
///   <para>
///     The data provided by the TX_ASYNCDATACALLBACK will contain a result
///     code which can be retrieved using txGetAsyncDataResult(). The result
///     code will be one of the following: <br /><br />TX_RESULT_OK: <br />
///     The profile was successfully set. <br /><br />TX_RESULT_NOTFOUND: <br />
///     The profile was not found among the available profiles. <br />
///   </para>
///   <para>
///     The handle to the async data must NOT be released.
///   </para>
/// </param>
/// <param name="userParam">
///   <para>
///     A TX_USERPARAM which will be provided as a parameter to the
///     completion callback. <br />
///   </para>
///   <para>
///     Can be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The command was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txDeleteProfile(
  hContext: TX_CONTEXTHANDLE;
  profileName: TX_CONSTSTRING;
  completionHandler: TX_ASYNCDATACALLBACK;
  userParam: TX_USERPARAM): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


// EyeXBehavior.h

/// <summary>
///   Gets the TX_BEHAVIORTYPE of an interaction behavior.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="BehaviorType">
///   A TX_BEHAVIORTYPE which will be set to the type of the behavior <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The type of the behavior was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txGetBehaviorType(
  hBehavior: TX_CONSTHANDLE;
  var BehaviorType: TX_BEHAVIORTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets TX_ACTIVATABLEPARAMS for an activatable Behavior. <br />
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_HANDLE to the behavior on which to set the parameters. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Params">
///   A TX_ACTIVATABLEPARAMS which specifies the behaviors parameters. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The option was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetActivatableBehaviorParams(
  hBehavior: TX_HANDLE;
  var Params: TX_ACTIVATABLEPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_ACTIVATABLEPARAMS for an activatable behavior. <br />
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the parameters should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Params">
///   A TX_ACTIVATABLEPARAMS which will be set to the behaviors parameters. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The parameters was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   was not of type TX_BEHAVIORTYPE_ACTIVATABLE.
/// </returns>
function txGetActivatableBehaviorParams(
  hBehavior: TX_CONSTHANDLE;
  var Params: TX_ACTIVATABLEPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_ACTIVABLEEVENTTYPE for an activatable behavior.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the event type will be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="EventType">
///   A TX_ACTIVATABLEEVENTTYPE which will be set to the event type.
/// </param>
/// <returns>
///   TX_RESULT_OK: The event type was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   was not of type TX_BEHAVIORTYPE_ACTIVATABLE.
/// </returns>
function txGetActivatableEventType(
  hBehavior: TX_CONSTHANDLE;
  var EventType: TX_ACTIVATABLEEVENTTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_ACTIVATIONFOCUSCHANGEDEVENTPARAMS for an activatable
///   behavior.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the event parameters will
///     be retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="EventParams">
///   A TX_ACTIVATIONFOCUSCHANGEDEVENTPARAMS which will be set to the behaviors
///   event parameters. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The parameters was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   was not of type TX_BEHAVIORTYPE_ACTIVATABLE. <br />TX_RESULT_NOTFOUND:
///   The options could not be found due to invalid event type.
/// </returns>
function txGetActivationFocusChangedEventParams(
  hBehavior: TX_CONSTHANDLE;
  var EventParams: TX_ACTIVATIONFOCUSCHANGEDEVENTPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets the TX_PANNABLEPARAMS for a pannable behavior.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_HANDLE to the behavior on which to set the parameters. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Params">
///   A pointer to a TX_PANNABLEPARAMS which specifies the parameters.
/// </param>
/// <returns>
///   TX_RESULT_OK: The parameters was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   was not of type TX_BEHAVIORTYPE_PANNABLE.
/// </returns>
function txSetPannableBehaviorParams(
  hBehavior: TX_HANDLE;
  var Params: TX_PANNABLEPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_PANNABLEPARAMS for a pannable behavior.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the parameters will be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Params">
///   A TX_PANNABLEPARAMS which will be set to the parameters.
/// </param>
/// <returns>
///   TX_RESULT_OK: The parameters was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   was not of type TX_BEHAVIORTYPE_PANNABLE.
/// </returns>
function txGetPannableBehaviorParams(
  hBehavior: TX_CONSTHANDLE;
  var Params: TX_PANNABLEPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_PANNABLEEVENTTYPE for a pannable behavior event.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the event type will be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="EventType">
///   A TX_PANNABLEEVENTTYPE which will be set to the event type. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The event type was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   was not of type TX_BEHAVIORTYPE_PANNABLE.
/// </returns>
function txGetPannableEventType(
  hBehavior: TX_CONSTHANDLE;
  var EventType: TX_PANNABLEEVENTTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_PANNABLEPANEVENTPARAMS for a pannable behavior pan event.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the parameters will be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="EventParams">
///   A TX_PANNABLEPANEVENTPARAMS which will be set to the parameters.
/// </param>
/// <returns>
///   TX_RESULT_OK: The event parameters was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   was not of type TX_BEHAVIORTYPE_PANNABLE.
/// </returns>
function txGetPannablePanEventParams(
  hBehavior: TX_CONSTHANDLE;
  var EventParams: TX_PANNABLEPANEVENTPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_PANNABLESTEPEVENTPARAMS for a pannable behavior step event.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the parameters will be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE. <br />
///   </para>
/// </param>
/// <param name="EventParams">
///   A TX_PANNABLESTEPEVENTPARAMS which will be set to the parameters.
/// </param>
/// <returns>
///   TX_RESULT_OK: The event parameters was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   was not of type TX_BEHAVIORTYPE_PANNABLE.
/// </returns>
function txGetPannableStepEventParams(
  hBehavior: TX_CONSTHANDLE;
  var EventParams: TX_PANNABLESTEPEVENTPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_PANNABLEHANDSFREEEVENTPARAMS for a pannable behavior hands
///   free event.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the parameters will be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="EventParams">
///   A TX_PANNABLEHANDSFREEEVENTPARAMS which will be set to the parameters.
/// </param>
/// <returns>
///   TX_RESULT_OK: The event parameters was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   was not of type TX_BEHAVIORTYPE_PANNABLE.
/// </returns>
function txGetPannableHandsFreeEventParams(
  hBehavior: TX_CONSTHANDLE;
  var EventParams: TX_PANNABLEHANDSFREEEVENTPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets TX_GAZEPOINTDATAPARAMS for a gaze point data behavior.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_HANDLE to the behavior on which to set the parameters. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Params">
///   A TX_GAZEPOINTDATAPARAMS which specifies the behavior parameters.
/// </param>
/// <returns>
///   TX_RESULT_OK: The parameters was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   was not of type TX_BEHAVIORTYPE_GAZEPOINTDATA.
/// </returns>
function txSetGazePointDataBehaviorParams(
  hBehavior: TX_HANDLE;
  var Params: TX_GAZEPOINTDATAPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_GAZEPOINTDATAPARAMS for gaze point data behavior.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the parameter will be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Params">
///   A TX_GAZEPOINTDATAPARAMS which will be set to the behavior parameters. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The parameters was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   was not of type TX_BEHAVIORTYPE_GAZEPOINTDATA.
/// </returns>
function txGetGazePointDataBehaviorParams(
  hBehavior: TX_CONSTHANDLE;
  var Params: TX_GAZEPOINTDATAPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_GAZEPOINTDATAEVENTPARAMS for a gaze point behavior.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the event parameters will
///     be retrieved.
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="EventParams">
///   A pointer to a TX_GAZEPOINTDATAEVENTPARAMS which will be set to the
///   behavior event parameters.
/// </param>
/// <returns>
///   TX_RESULT_OK: The event parameters was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   is not of type TX_BEHAVIORTYPE_GAZEPOINTDATA.
/// </returns>
function txGetGazePointDataEventParams(
  hBehavior: TX_CONSTHANDLE;
  var EventParams: TX_GAZEPOINTDATAEVENTPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets TX_GAZEAWAREPARAMS for a gaze aware behavior.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_HANDLE to the behavior on which to set the parameters. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Params">
///   A pointer to a TX_GAZEAWAREPARAMS which specifies the behavior
///   parameters.
/// </param>
/// <returns>
///   TX_RESULT_OK: The parameters was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   was not of type TX_BEHAVIORTYPE_GAZEAWARE.
/// </returns>
function txSetGazeAwareBehaviorParams(
  hBehavior: TX_HANDLE;
  var Params: TX_GAZEAWAREPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_GAZEAWAREPARAMS for gaze point data behavior.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the parameter will be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Params">
///   A TX_GAZEAWAREPARAMS which will be set to the behavior parameters. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The parameters was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   was not of type TX_BEHAVIORTYPE_GAZEAWARE.
/// </returns>
function txGetGazeAwareBehaviorParams(
  hBehavior: TX_CONSTHANDLE;
  var Params: TX_GAZEAWAREPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_GAZEAWAREEVENTPARAMS for a gaze-aware behavior.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the event parameters will
///     be retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="EventParams">
///   A TX_GAZEAWAREEVENTPARAMS which will be set to the behavior event
///   parameters. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The parameters was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   was not of type TX_BEHAVIORTYPE_GAZEAWARE.
/// </returns>
function txGetGazeAwareBehaviorEventParams(
  hBehavior: TX_CONSTHANDLE;
  var EventParams: TX_GAZEAWAREEVENTPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_FIXATIONDATAPARAMS for a fixation behavior.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_HANDLE to the behavior on which the parameters will be set. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Params">
///   A TX_FIXATIONDATAPARAMS which will be set to the behavior parameters.
/// </param>
/// <returns>
///   TX_RESULT_OK: The parameters was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   is not of type TX_Behavior_FIXATIONDATA.
/// </returns>
function txSetFixationDataBehaviorParams(
  hBehavior: TX_HANDLE;
  var Params: TX_FIXATIONDATAPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_FIXATIONDATAPARAMS for a fixation behavior.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the parameters will be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Params">
///   A TX_FIXATIONDATAPARAMS which will be set to the behavior parameters.
/// </param>
/// <returns>
///   TX_RESULT_OK: The parameters was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   is not of type TX_Behavior_FIXATIONDATA.
/// </returns>
function txGetFixationDataBehaviorParams(
  hBehavior: TX_CONSTHANDLE;
  var Params: TX_FIXATIONDATAPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_FIXATIONDATAEVENTPARAMS for a fixation behavior.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the event parameters will
///     be retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="EventParams">
///   A TX_FIXATIONDATAEVENTPARAMS which will be set to the behavior event
///   parameters.
/// </param>
/// <returns>
///   TX_RESULT_OK: The event parameters was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   is not of type TX_BEHAVIORTYPE_FIXATIONDATA.
/// </returns>
function txGetFixationDataEventParams(
  hBehavior: TX_CONSTHANDLE;
  var EventParams: TX_FIXATIONDATAEVENTPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_EYEPOSITIONDATAEVENTPARAMS for an eye position behavior.
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the event parameters will
///     be retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="EventParams">
///   A TX_EYEPOSITIONDATAEVENTPARAMS which will be set to the behavior event
///   parameters.
/// </param>
/// <returns>
///   TX_RESULT_OK: The event parameters was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDBEHAVIORTYPE: The behavior
///   is not of type TX_BEHAVIORTYPE_EYEPOSITIONDATA.
/// </returns>
function txGetEyePositionDataEventParams(
  hBehavior: TX_CONSTHANDLE;
  var EventParams: TX_EYEPOSITIONDATAEVENTPARAMS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the timestamp of when the behavior was created, in milliseconds. Not
///   to be confused with data timestamps, which deals with when the data was
///   captured by the tracker (e.g. TX_GAZEPOINTDATAEVENTPARAMS - Timestamp) <br />
/// </summary>
/// <param name="hBehavior">
///   <para>
///     A TX_CONSTHANDLE to the behavior from which the timestamp will be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Timestamp">
///   A TX_REAL which will be set to the behavior event Timestamp. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The timestamp was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: The timestamp cannot be
///   found for the supplied behavior.
/// </returns>
function txGetBehaviorEventTimestamp(
  hBehavior: TX_CONSTHANDLE;
  var Timestamp: TX_REAL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


 // EyeXStates.h

/// <summary>
///   Gets a state from the client. The state will be delivered as a TX_HANDLE
///   to a state bag. The handle will be <br />TX_EMPTY_HANDLE if the requested
///   state was not found. <br />
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context from which to get the state. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="statePath">
///   A string that specifies the path of the state to get. <br />Must not
///   start with, end with or have two consecutive dots (.). <br />Must not be
///   nil or empty string. <br />
/// </param>
/// <param name="completionHandler">
///   <para>
///     The TX_ASYNCDATACALLBACK that will be invoked when the result have
///     arrived from the client. Must not be NULL.
///   </para>
///   <para>
///     The data provided by the TX_ASYNCDATACALLBACK will contain a result
///     code which can be retrieved using txGetAsyncDataResult(). The result
///     code will be one of the following: <br /><br />TX_RESULT_OK: <br />
///     The state was successfully retrieved. <br /><br />TX_RESULT_NOTFOUND:
///     <br />The state was not found. <br /><br />TX_RESULT_CANCELLED: <br />
///     The asynchronous operation was canceled. <br />
///   </para>
/// </param>
/// <param name="userParam">
///   <para>
///     A TX_USERPARAM which will be provided as a parameter to the
///     completion callback. <br />
///   </para>
///   <para>
///     Can be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The state request was successfully sent to the client. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txGetStateAsync(
  hContext: TX_CONTEXTHANDLE;
  statePath: TX_CONSTSTRING;
  completionHandler: TX_ASYNCDATACALLBACK;
  userParam: TX_USERPARAM
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets a state from the client synchronously. <br />This method will block
///   until the state has been retrieved or until the operation has failed.
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context from which to get the state. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="statePath">
///   A string that specifies the path of the state to get. <br />Must not
///   start with, end with or have two consecutive dots (.). <br />Must not be
///   NULL or empty string. <br />
/// </param>
/// <param name="hStateBag">
///   <para>
///     A TX_HANDLE which will be set to the state bag. <br />
///   </para>
///   <para>
///     Will be set to TX_EMPTY_HANDLE if not found.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The state was succcessfully retrieved. <br />
///   TX_RESULT_NOTFOUND: The state was not found. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_CANCELLED: The operation was
///   cancelled.
/// </returns>
function txGetState(
  hContext: TX_CONTEXTHANDLE;
  statePath: TX_CONSTSTRING;
  var hStateBag: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets a state on the client. For internal use only.
/// </summary>
/// <param name="hStateBag">
///   A handle to the state bag which contains the path and data to set.
/// </param>
/// <param name="completionHandler">
///   The TX_ASYNCDATACALLBACK that will be invoked when the result have
///   arrived from the client. Can be NULL to ignore the result. <br /><br />
///   The data provided by the TX_ASYNCDATACALLBACK will contain a result code
///   which can be retrieved using txGetAsyncDataResult(). The result code will
///   be one of the following: <br /><br />TX_RESULT_OK: <br />The state was
///   successfully set. <br /><br />TX_RESULT_CANCELLED: <br />The asynchronous
///   operation was canceled
/// </param>
/// <param name="userParam">
///   <para>
///     A TX_USERPARAM which will be provided as a parameter to the
///     completion callback. <br />
///   </para>
///   <para>
///     Can be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The set state request was successfully sent to the client. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetStateAsync(
  hStateBag: TX_HANDLE;
  completionHandler: TX_ASYNCDATACALLBACK;
  userParam: TX_USERPARAM
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Creates a state bag.
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context on which to create the state bag. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hStateBag">
///   A TX_HANDLE which will be set to the newly created state bag. <br />This
///   handle must be released using txReleaseObject to avoid leaks. <br />The
///   value must be set to TX_EMPTY_HANDLE. <br />
/// </param>
/// <param name="statePath">
///   A string that specifies which path this state bag represents. <br />Must
///   not start with, end with or have two consecutive dots (.). <br />Must not
///   be NULL or empty string.
/// </param>
/// <returns>
///   TX_RESULT_OK: The state bag was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txCreateStateBag(
  hContext: TX_CONTEXTHANDLE;
  var hStateBag: TX_HANDLE;
  statePath: TX_CONSTSTRING
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the path that a state bag represents. <br />
/// </summary>
/// <param name="hStateBag">
///   <para>
///     A TX_CONSTHANDLE to the state bag. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="pStatePath">
///   A TX_STRING to which the state path will be copied. <br />Must be at
///   least the size of the value. <br />Can be NULL to only get the size of
///   the path. <br />
/// </param>
/// <param name="StatePathSize">
///   A pointer to a TX_SIZE which will be set to the size of the state path. <br />
///   Must not be NULL. <br />The value must be 0 if pStatePath is NULL.
/// </param>
/// <returns>
///   TX_RESULT_OK: The path of the state bag or the required size of the
///   string was successfully retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED:
///   The EyeX client environment is not initialized. <br />
///   TX_RESULT_INVALIDARGUMENT: An invalid argument was passed to the
///   function. <br />TX_RESULT_INVALIDBUFFERSIZE: The size of pStatePath is
///   invalid (*pStatePathSize will be set to the required size).
/// </returns>
function txGetStateBagPath(
  hStateBag: TX_CONSTHANDLE;
  pStatePath: TX_STRING;
  var StatePathSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Registers observation of a specified state path. <br />If connection to
///   the client is currently not present this registration will be stored and
///   applied once connection has been established. The registration will also
///   be reapplied if the connection is dropped and reestablished. <br />
///   Multiple registrations of the same state path will be ignored.
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context on which to register the state
///     observation. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="statePath">
///   The state path as a TX_CONSTSTRING. <br />Must not start with, end with
///   or have two consecutive dots (.). <br />Must not be NULL or empty string.
/// </param>
/// <returns>
///   TX_RESULT_OK: The state path was successfully registered. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDTHREAD: Attempted to call
///   the function from a callback thread.
/// </returns>
function txRegisterStateObserver(
  hContext: TX_CONTEXTHANDLE;
  statePath: TX_CONSTSTRING
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Unregisters observation of a specified state path.
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context on which to unregister the state
///     observation. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="statePath">
///   The state path as a TX_CONSTSTRING. <br />Must not start with, end with
///   or have two consecutive dots (.). <br />Must not be NULL or empty string.
/// </param>
/// <returns>
///   TX_RESULT_OK: The state path was successfully unregistered. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOT_FOUND: The state path was not
///   observed.
/// </returns>
function txUnregisterStateObserver(
  hContext: TX_CONTEXTHANDLE;
  statePath: TX_CONSTSTRING
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets a value from a state bag as a TX_INTEGER. <br />If a state value can
///   not be found on the specified path or the value is of another type this
///   call will fail.
/// </summary>
/// <param name="hStateBag">
///   <para>
///     A TX_CONSTHANDLE to the state bag from which the value should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="valuePath">
///   The path to the value.
/// </param>
/// <param name="IntValue">
///   A TX_INTEGER which will be set to the value. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The state value was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: The value was not
///   found. <br />TX_RESULT_INVALIDPROPERTYTYPE: The value type was not
///   TX_PROPERTYVALUETYPE_INTEGER.
/// </returns>
function txGetStateValueAsInteger(
  hStateBag: TX_CONSTHANDLE;
  valuePath: TX_CONSTSTRING;
  var IntValue: TX_INTEGER
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets a value from a state bag as a TX_REAL. <br />If a state value can
///   not be found on the specified path or the value is of another type this
///   call will fail. <br />
/// </summary>
/// <param name="hStateBag">
///   <para>
///     A TX_CONSTHANDLE to the state bag from which the value should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="valuePath">
///   The path to the value.
/// </param>
/// <param name="RealValue">
///   A TX_REAL which will be set to the value. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The state value was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: The value was not
///   found. <br />TX_RESULT_INVALIDPROPERTYTYPE: The value type was not
///   TX_PROPERTYVALUETYPE_REAL.
/// </returns>
function txGetStateValueAsReal(
  hStateBag: TX_CONSTHANDLE;
  valuePath: TX_CONSTSTRING;
  var RealValue: TX_REAL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets a value from a state bag as a TX_STRING. <br />If a state value can
///   not be found on the specified path or the value is of another type this
///   call will fail.
/// </summary>
/// <param name="hStateBag">
///   <para>
///     A TX_CONSTHANDLE to the state bag from which the value should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="valuePath">
///   The path to the value.
/// </param>
/// <param name="pStringValue">
///   A TX_STRING to which the state value will be copied. <br />Must be at
///   least the size of the value. <br />Can be NULL to only get the size of
///   the value. <br />If the state value is an array of strings the returned
///   string will contain all strings separated with null termination and with
///   an extra ending null termination. <br />
/// </param>
/// <param name="StringSize">
///   A pointer to a TX_SIZE which will be set to the size of the state value. <br />
///   Must not be NULL. <br />The value must be 0 if pStringValue is NULL.
/// </param>
/// <returns>
///   TX_RESULT_OK: The state value or the required size of the string was
///   successfully retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT:
///   An invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of pStringValue is invalid
///   (*pStringSize will be set to the required size). <br />
///   TX_RESULT_NOTFOUND: The value was not found. <br />
///   TX_RESULT_INVALIDPROPERTYTYPE: The value type was not
///   TX_PROPERTYVALUETYPE_STRING or array of TX_PROPERTYVALUETYPE_STRING.
/// </returns>
function txGetStateValueAsString(
  hStateBag: TX_CONSTHANDLE;
  valuePath: TX_CONSTSTRING;
  pStringValue: TX_STRING;
  var StringSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets a value from a state bag as a TX_RECT. <br />If a state value can
///   not be found on the specified path or the value is of another type this
///   call will fail.
/// </summary>
/// <param name="hStateBag">
///   <para>
///     A TX_CONSTHANDLE to the state bag from which the value should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="valuePath">
///   The path to the value. <br />
/// </param>
/// <param name="RectValue">
///   A TX_RECT which will have its members set.
/// </param>
/// <returns>
///   TX_RESULT_OK: The state value was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: The value was not
///   found.
/// </returns>
function txGetStateValueAsRectangle(
  hStateBag: TX_CONSTHANDLE;
  valuePath: TX_CONSTSTRING;
  var RectValue: TX_RECT
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets a value from a state bag as a TX_VECTOR2. <br />If a state value can
///   not be found on the specified path or the value is of another type this
///   call will fail.
/// </summary>
/// <param name="hStateBag">
///   <para>
///     A TX_CONSTHANDLE to the state bag from which the value should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="valuePath">
///   The path to the value.
/// </param>
/// <param name="Vector2Value">
///   <para>
///     A pointer to a TX_VECTOR2 which will have its members set. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The state value was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: The value was not
///   found.
/// </returns>
function txGetStateValueAsVector2(
  hStateBag: TX_CONSTHANDLE;
  valuePath: TX_CONSTSTRING;
  var Vector2Value: TX_VECTOR2
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <param name="hStateBag">
///   Gets a value from a state bag as a TX_SIZE2. <br />If a state value can
///   not be found on the specified path or the value is of another type this
///   call will fail.
/// </param>
/// <param name="valuePath">
///   The path to the value.
/// </param>
/// <param name="SizeValue">
///   A TX_SIZE2 which will have its members set. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The state value was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: The value was not
///   found.
/// </returns>
function txGetStateValueAsSize2(
  hStateBag: TX_CONSTHANDLE;
  valuePath: TX_CONSTSTRING;
  var SizeValue: TX_SIZE2
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets a value in a state bag to a TX_INTEGER. <br />
/// </summary>
/// <param name="hStateBag">
///   <para>
///     A TX_HANDLE to the state bag from which the value should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="valuePath">
///   The path to the value.
/// </param>
/// <param name="intValue">
///   A TX_INTEGER which is the value to set.
/// </param>
/// <returns>
///   <para>
///     TX_RESULT_OK: The state value was successfully set.
///   </para>
///   <para>
///     TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///     initialized. TX_RESULT_INVALIDARGUMENT: An invalid argument was
///     passed to the function. <br />
///   </para>
/// </returns>
function txSetStateValueAsInteger(
  hStateBag: TX_HANDLE;
  valuePath: TX_CONSTSTRING;
  intValue: TX_INTEGER
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets a value in a state bag to a TX_REAL.
/// </summary>
/// <param name="hStateBag">
///   <para>
///     A TX_HANDLE to the state bag from which the value should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="valuePath">
///   The path to the value.
/// </param>
/// <param name="realValue">
///   A TX_REAL which is the value to set.
/// </param>
/// <returns>
///   TX_RESULT_OK: The state value was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetStateValueAsReal(
  hStateBag: TX_HANDLE;
  valuePath: TX_CONSTSTRING;
  realValue: TX_REAL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets a value in a state bag to a string. <br />
/// </summary>
/// <param name="hStateBag">
///   <para>
///     A TX_HANDLE to the state bag from which the value should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="valuePath">
///   The path to the value.
/// </param>
/// <param name="stringValue">
///   A TX_CONSTSTRING which is the value to set.
/// </param>
/// <returns>
///   TX_RESULT_OK: The state value was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetStateValueAsString(
  hStateBag: TX_HANDLE;
  valuePath: TX_CONSTSTRING;
  stringValue: TX_CONSTSTRING
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets a value in a state bag to a TX_RECT. <br />
/// </summary>
/// <param name="hStateBag">
///   <para>
///     A TX_HANDLE to the state bag from which the value should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="valuePath">
///   The path to the value.
/// </param>
/// <param name="RectValue">
///   A TX_RECT which is the value to set.
/// </param>
/// <returns>
///   TX_RESULT_OK: The state value was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetStateValueAsRectangle(
  hStateBag: TX_HANDLE;
  valuePath: TX_CONSTSTRING;
  var RectValue: TX_RECT
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets a value in a state bag to a TX_VECTOR2.
/// </summary>
/// <param name="hStateBag">
///   <para>
///     A TX_HANDLE to the state bag from which the value should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="valuePath">
///   The path to the value.
/// </param>
/// <param name="Vector2Value">
///   A TX_VECTOR2 which is the value to set.
/// </param>
/// <returns>
///   TX_RESULT_OK: The state value was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetStateValueAsVector2(
  hStateBag: TX_HANDLE;
  valuePath: TX_CONSTSTRING;
  var Vector2Value: TX_VECTOR2
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets a value in a state bag to a TX_SIZE2.
/// </summary>
/// <param name="hStateBag">
///   <para>
///     A TX_HANDLE to the state bag from which the value should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="valuePath">
///   The path to the value.
/// </param>
/// <param name="SizeValue">
///   A TX_SIZE2 which is the value to set.
/// </param>
/// <returns>
///   TX_RESULT_OK: The state value was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetStateValueAsSize2(
  hStateBag: TX_HANDLE;
  valuePath: TX_CONSTSTRING;
  var SizeValue: TX_SIZE2
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets a property for a specified state value. <br />If the property for
///   state value can not be found on the specified path or the value is of
///   another type this call will fail. <br />
/// </summary>
/// <param name="hStateBag">
///   <para>
///     A TX_CONSTHANDLE to the state bag from which the value should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hProperty">
///   A TX_PROPERTYHANDLE which will be set to the property for the state
///   value.
/// </param>
/// <param name="valuePath">
///   The path to the value.
/// </param>
/// <param name="createIfNotFound">
///   Specifies if the property should be created if it does not exist.
/// </param>
/// <returns>
///   TX_RESULT_OK: The state value was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: The property was not
///   found.
/// </returns>
function txGetPropertyForStateValue(
  hStateBag: TX_CONSTHANDLE;
  var hProperty: TX_PROPERTYHANDLE;
  valuePath: TX_CONSTSTRING;
  createIfNotFound: TX_BOOL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Registers a state changed handler. <br />This is a helper which
///   automatically registers a notification message handler and a state
///   observer. <br />
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context on which to listen for state
///     changes. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="Ticket">
///   A TX_TICKET which will represent this registration. <br />This ticket
///   should be used for unregistration. <br />
/// </param>
/// <param name="statePath">
///   A string that specifies the path of the state to register state changed
///   handler for. <br />Must not start with, end with or have two consecutive
///   dots (.). <br />Must not be NULL or empty string. <br />
/// </param>
/// <param name="handler">
///   A TX_ASYNCDATACALLBACK which will be called when a state changes.
/// </param>
/// <param name="userParam">
///   A TX_USERPARAM which will be provided as a parameter to the callback
/// </param>
/// <returns>
///   TX_RESULT_OK: The Query Handler was successfully registered. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. TX_RESULT_INVALIDARGUMENT: An invalid argument was passed to
///   the function. TX_RESULT_INVALIDTHREAD: Attempted to call the function
///   from a callback thread.
/// </returns>
function txRegisterStateChangedHandler(
  hContext: TX_CONTEXTHANDLE;
  var Ticket: TX_TICKET;
  statePath: TX_CONSTSTRING;
  handler: TX_ASYNCDATACALLBACK;
  userParam: TX_USERPARAM): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Unregisters a previously registered state changed handler callback.
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context on which to unregister the
///     callback. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="ticket">
///   A TX_TICKET which represents the registration. <br />Must not be
///   TX_INVALID_TICKET
/// </param>
/// <returns>
///   <para>
///     TX_RESULT_OK: The callback was successfully unregistered. <br />
///     TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///     initialized. TX_RESULT_INVALIDARGUMENT: An invalid argument was
///     passed to the function.
///   </para>
///   <para>
///     TX_RESULT_NOTFOUND: A registration for the specified ticket could not
///     be found.
///   </para>
/// </returns>
function txUnregisterStateChangedHandler(
  hContext: TX_CONTEXTHANDLE;
  ticket: TX_TICKET): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


 // EyeXNotification.h

/// <summary>
///   Gets the TX_NOTIFICATIONTYPE of a notification.
/// </summary>
/// <param name="hNotification">
///   <para>
///     A TX_CONSTHANDLE to the notification. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="NotificationType">
///   A TX_NOTIFICATIONTYPE which will be set to the type of the notification. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The type of the notification was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txGetNotificationType(
  hNotification: TX_CONSTHANDLE;
  var NotificationType: TX_NOTIFICATIONTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the data of a notification.
/// </summary>
/// <param name="hNotification">
///   <para>
///     A TX_CONSTHANDLE to the notification. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hObject">
///   A pointer to a TX_HANDLE to which the handle of the object used as data
///   will be copied. <br />This handle must be released using txReleaseObject
///   to avoid leaks. <br />The value of the pointer must be set to
///   TX_EMPTY_HANDLE.
/// </param>
/// <returns>
///   TX_RESULT_OK: The data of the notification was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: The notification does
///   not have any data.
/// </returns>
function txGetNotificationData(
  hNotification: TX_CONSTHANDLE;
  var hObject: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


 // EyeXQuery.h

/// <summary>
///   Gets the bounds of a query. <br />
/// </summary>
/// <param name="hQuery">
///   <para>
///     A TX_CONSTHANDLE to the query from which the bounds should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hBounds">
///   A pointer to a TX_HANDLE which will be set to the bounds of the
///   interactor. <br />This handle must be released using txReleaseObject to
///   avoid leaks. <br />The value of the pointer must be set to
///   TX_EMPTY_HANDLE. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The bounds was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: This query does not
///   have any bounds.
/// </returns>
function txGetQueryBounds(
  hQuery: TX_CONSTHANDLE;
  var hBounds: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the number of window ids held by a query. <br />The client is
///   expected to add interactors to a snapshot for the windows specified in
///   the query, and also report these window id's in the snapshot, regardless
///   of if any interactors are found for that window.
/// </summary>
/// <param name="hQuery">
///   <para>
///     A TX_CONSTHANDLE to the query for which the number of window ids
///     should be retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="WindowIdsCount">
///   A TX_SIZE which will be set the number of window ids.
/// </param>
/// <returns>
///   TX_RESULT_OK: The number of window ids was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txGetQueryWindowIdCount(
  hQuery: TX_CONSTHANDLE;
  var WindowIdsCount: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets one of the window ids held by a query. Which one is specified by an
///   index. <br />The client is expected to add interactors to a snapshot for
///   the windows specified in the query, and also report these window id's in
///   the snapshot, regardless of if any interactors are found for that window.
/// </summary>
/// <param name="hQuery">
///   <para>
///     A TX_CONSTHANDLE to the query for which the window id should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="windowIdIndex">
///   The index of the window id to get. <br />Must be a positive integer. <br />
/// </param>
/// <param name="pWindowId">
///   A TX_STRING to which the window id will be copied. <br />Must be at least
///   the size of the window id. <br />Can be nil to only get the size of the
///   window id. <br />
/// </param>
/// <param name="WindowIdSize">
///   A pointer to a TX_SIZE which tells the size of pWindowId. <br />Will be
///   set the size of the window id. <br />The value must be 0 if pWindowId is
///   NIL.
/// </param>
/// <returns>
///   TX_RESULT_OK: The window id or the required size of the string was
///   successfully retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT:
///   An invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of windowId is invalid
///   (pWindowIdSize will be set to the required size). <br />
///   TX_RESULT_NOTFOUND: The specified index was out of range.
/// </returns>
function txGetQueryWindowId(
  hQuery: TX_CONSTHANDLE;
  windowIdIndex: TX_INTEGER;
  pWindowId: TX_STRING;
  var WindowIdSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


 // EyeXEvent.h

/// <summary>
///   Gets the id of the interactor for which the event should apply. <br />
/// </summary>
/// <param name="hEvent">
///   <para>
///     A TX_CONSTHANDLE to the event from which the interactor id should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="pInteractorId">
///   A TX_STRING to which the interactor id will be copied. <br />Must be at
///   least the size of the interactor id. <br />Can be nil to only get the
///   size of the interactor. <br />
/// </param>
/// <param name="InteractorIdSize">
///   A TX_SIZE which will be set to the size of the interactor id. <br />The
///   value must be 0 if pInteractorId is NULL. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The interactor id or the required size of the string was
///   successfully retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT:
///   An invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of pInteractorId is invalid
///   (*pInteractorIdSize will be set to the required size).
/// </returns>
function txGetEventInteractorId(
  hEvent: TX_CONSTHANDLE;
  pInteractorId: TX_STRING;
  var InteractorIdSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets a behavior with a specified TX_BEHAVIORTYPE from an event. <br />If
///   the event does not have a behavior of the specified type this call will
///   fail. <br />
/// </summary>
/// <param name="hEvent">
///   <para>
///     A TX_CONSTHANDLE to the event from which the behavior should be
///     retrieved. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hBehavior">
///   A TX_HANDLE which will be set to the behavior. <br />This handle must be
///   released using txReleaseObject to avoid leaks. <br />The value of the
///   pointer must be set to TX_EMPTY_HANDLE.
/// </param>
/// <param name="behaviorType">
///   The TX_BEHAVIORTYPE which specifies what type of behavior to get.
/// </param>
/// <returns>
///   TX_RESULT_OK: The behavior was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_NOTFOUND: This event does not
///   have a behavior of the specified type.
/// </returns>
function txGetEventBehavior(
  hEvent: TX_CONSTHANDLE;
  var hBehavior: TX_HANDLE;
  behaviorType: TX_BEHAVIORTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_HANDLEs to all the behaviors on an event. <br />
/// </summary>
/// <param name="hEvent">
///   <para>
///     A TX_CONSTHANDLE to the event from which to get the behaviors. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="phBehaviors">
///   A pointer to an array of TX_HANDLEs to which the behavior handles will be
///   copied. <br />These handles must be released using txReleaseObject to
///   avoid leaks. <br />Can be Nil to only get the required size. <br />
/// </param>
/// <param name="BehaviorsSize">
///   A pointer to a TX_SIZE which will be set to the number of behaviors. <br />
///   Must not be NULL. <br />The value must be 0 if phBehaviors is Nil.
/// </param>
/// <returns>
///   TX_RESULT_OK: The handles or the required size of the buffer was
///   retrieved successfully. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT:
///   An invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of the array is invalid.
///   (*pBehaviorsSize will be set to the number of behaviors).
/// </returns>
function txGetEventBehaviors(
  hEvent: TX_CONSTHANDLE;
  phBehaviors: PTXHANDLE;
  var BehaviorsSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}



// EyeXProperty.h

/// <summary>
///   Creates a property bag. <br />
/// </summary>
/// <param name="hContext">
///   <para>
///     A TX_CONTEXTHANDLE to the context on which to create the property
///     bag. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hBag">
///   A TX_HANDLE which will be set to the newly created property bag. <br />
///   This handle must be released using txReleaseObject to avoid leaks. <br />
///   The value must be set to TX_EMPTY_HANDLE. <br />
/// </param>
/// <param name="txtype">
///   A TX_PROPERTYBAGTYPE which specifies what type of property bag to create.
/// </param>
/// <returns>
///   TX_RESULT_OK: The property bag was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txCreatePropertyBag(
  hContext: TX_CONTEXTHANDLE;
  var hBag: TX_HANDLE;
  txtype: TX_PROPERTYBAGTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}

/// <summary>
///   Gets the TX_PROPERTYBAGTYPE of a property bag. <br />
/// </summary>
/// <param name="hBag">
///   <para>
///     A TX_CONSTHANDLE to the property bag. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="BagType">
///   <para>
///     A pointer to a TX_PROPERTYBAGTYPE which will be set to the type of
///     the property bag. <br />
///   </para>
///   <para>
///     Must not be NULL.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The type of the property bag was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txGetPropertyBagType(
  hBag: TX_CONSTHANDLE;
  var BagType: TX_PROPERTYBAGTYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Creates a property on an interaction object.
/// </summary>
/// <param name="hObject">
///   <para>
///     A TX_HANDLE to the object on which to create the property. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hProperty">
///   A TX_PROPERTYHANDLE which will be set to the newly created property. <br />
///   The value of the pointer must be set to TX_EMPTY_HANDLE. <br />
/// </param>
/// <param name="propertyName">
///   The name of the property. <br />Can be NULL or empty only if the object
///   is of type TX_INTERACTIONOBJECTTYPE_PROPERTYBAG with bag type
///   TX_PROPERTYBAGTYPE_ARRAY. <br />
/// </param>
/// <returns>
///   <para>
///     TX_RESULT_OK: The property was successfully created.
///   </para>
///   <para>
///     TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///     initialized. TX_RESULT_INVALIDARGUMENT: An invalid argument was
///     passed to the function. TX_RESULT_INVALIDPROPERTYNAME: The name of
///     the property was invalid.
///   </para>
///   <para>
///     TX_RESULT_DUPLICATEPROPERTY: There already exists a property with the
///     specified name on this object.
///   </para>
/// </returns>
function txCreateProperty(
  hObject: TX_HANDLE;
  var hProperty: TX_PROPERTYHANDLE;
  propertyName: TX_CONSTSTRING
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Removes a property from an interaction object. <br />
/// </summary>
/// <param name="hObject">
///   <para>
///     A TX_HANDLE to the object from which to remove the property. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="propertyName">
///   The name of the property to remove. <br />Must not be NULL or empty
///   string.
/// </param>
/// <returns>
///   <para>
///     TX_RESULT_OK: The property was successfully removed.
///   </para>
///   <para>
///     TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///     initialized. TX_RESULT_INVALIDARGUMENT: An invalid argument was
///     passed to the function.
///   </para>
///   <para>
///     TX_RESULT_NOTFOUND: A property with the specified name was not found.
///     TX_RESULT_PROPERTYNOTREMOVABLE: The specified property can not be
///     removed.
///   </para>
/// </returns>
function txRemoveProperty(
  hObject: TX_HANDLE;
  propertyName: TX_CONSTSTRING
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets a property from an interaction object.
/// </summary>
/// <param name="hObject">
///   <para>
///     A TX_CONSTHANDLE to the object from which to get the property. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="hProperty">
///   A TX_PROPERTYHANDLE which will be set to the property. <br />The value of
///   the pointer must be set to TX_EMPTY_HANDLE.
/// </param>
/// <param name="propertyName">
///   The name of the property to get. <br />Must not be NULL or empty string <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The property was successfully created. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDPROPERTYNAME: The name of
///   the property was invalid. <br />TX_RESULT_NOTFOUND: A property with the
///   specified name was not found.
/// </returns>
function txGetProperty(
  hObject: TX_CONSTHANDLE;
  var hProperty: TX_PROPERTYHANDLE;
  propertyName: TX_CONSTSTRING
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets TX_HANDLEs to all properties on an interaction object.
/// </summary>
/// <param name="hObject">
///   <para>
///     A TX_CONSTHANDLE to the object from which to get the properties. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="phProperties">
///   A pointer to an array of TX_PROPERTYHANDLEs to which the property handles
///   will be copied. <br />Can be Nil to only get the size. <br />
/// </param>
/// <param name="PropertiesSize">
///   <para>
///     A pointer to a TX_SIZE which will be set to the number of properties.
///     <br />Must not be Nil.
///   </para>
///   <para>
///     The value must be 0 if phProperties is Nil.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The handles or the required buffer size was retrieved
///   successfully. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX client
///   environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT: An
///   invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of the array is invalid.
///   (*pPropertiesSize will be set to the number of behaviors).
/// </returns>
function txGetProperties(
  hObject: TX_CONSTHANDLE;
  phProperties: TX_PROPERTYHANDLE;
  var PropertiesSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the name of a property. <br />
/// </summary>
/// <param name="hProperty">
///   <para>
///     A TX_CONSTPROPERTYHANDLE to the property for which to get the name. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="pName">
///   <para>
///     A TX_STRING to which the property name will be copied.
///   </para>
///   <para>
///     Must be at least the size of the property name. <br />Can be Nil to
///     only get the size of the property name. <br />
///   </para>
/// </param>
/// <param name="NameSize">
///   A pointer to a TX_SIZE which will be set to the size of the property
///   name. <br />The value must be 0 if pName is NULL. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The property name or the required size of the string was
///   successfully retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT:
///   An invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of pName is invalid (pNameSize will
///   be set to the required size).
/// </returns>
function txGetPropertyName(
  hProperty: TX_CONSTPROPERTYHANDLE;
  pName: TX_STRING;
  var NameSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


(**

  @param hProperty [in]:

  @param pPropertyType [out]:

  @return
 *)
/// <summary>
///   Gets the TX_PROPERTYVALUETYPE of a property.
/// </summary>
/// <param name="hProperty">
///   <para>
///     A TX_CONSTPROPERTYHANDLE to the property. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <param name="PropertyType">
///   A TX_PROPERTYVALUETYPE which will be set to the type of the value. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The type of the value was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txGetPropertyValueType(
  hProperty: TX_CONSTPROPERTYHANDLE;
  var PropertyType: TX_PROPERTYVALUETYPE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the TX_PROPERTYFLAGS of a property. <br />
/// </summary>
/// <param name="hProperty">
///   A TX_CONSTPROPERTYHANDLE to the property.
/// </param>
/// <param name="Flags">
///   A TX_PROPERTYFLAGS which will be set to the flags.
/// </param>
/// <returns>
///   TX_RESULT_OK: The flags were successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txGetPropertyFlags(
  hProperty: TX_CONSTPROPERTYHANDLE;
  var Flags: TX_PROPERTYFLAGS
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Clears the value of a property. <br />
/// </summary>
/// <param name="hProperty">
///   <para>
///     A TX_PROPERTYHANDLE to the property to clear. <br />
///   </para>
///   <para>
///     Must not be TX_EMPTY_HANDLE.
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The property was successfully cleared. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txClearPropertyValue(
  hProperty: TX_PROPERTYHANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets the value of a property to a TX_INTEGER. <br />If the property
///   already has a value that will be overwritten, regardless of type. <br />
///   The value type will be set to TX_PROPERTYVALUETYPE_INTEGER.
/// </summary>
/// <param name="hProperty">
///   A TX_PROPERTYHANDLE to the property for which the value should be set. <br />
///   Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="intValue">
///   A TX_INTEGER which is the value to set.
/// </param>
/// <returns>
///   TX_RESULT_OK: The property value was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetPropertyValueAsInteger(
  hProperty: TX_PROPERTYHANDLE;
  intValue: TX_INTEGER
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets the value of a property to a TX_REAL. <br />If the property already
///   has a value that will be overwritten, regardless of type. <br />The value
///   type will be set to TX_PROPERTYVALUETYPE_REAL.
/// </summary>
/// <param name="hProperty">
///   A TX_PROPERTYHANDLE to the property for which the value should be set. <br />
///   Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="realValue">
///   A TX_REAL which is the value to set.
/// </param>
/// <returns>
///   TX_RESULT_OK: The property value was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetPropertyValueAsReal(
  hProperty: TX_PROPERTYHANDLE;
  realValue: TX_REAL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets the value of a property to a string. <br />If the property already
///   has a value that will be overwritten, regardless of type. <br />The value
///   type will be set to TX_PROPERTYVALUETYPE_STRING. <br />
/// </summary>
/// <param name="hProperty">
///   A TX_PROPERTYHANDLE to the property for which the value should be set. <br />
///   Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="stringValue">
///   A TX_CONSTSTRING which is the value to set. <br />Must not be Nil. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The property value was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetPropertyValueAsString(
  hProperty: TX_PROPERTYHANDLE;
  stringValue: TX_CONSTSTRING
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets the value of a property to an interaction object. <br />If the
///   property already has a value that will be overwritten, regardless of
///   type. <br />The value type will be set to TX_PROPERTYVALUETYPE_OBJECT.
/// </summary>
/// <param name="hProperty">
///   A TX_PROPERTYHANDLE to the property for which the value should be set. <br />
///   Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="hObject">
///   A TX_HANDLE to the obejct which is the value to set. <br />Must not be
///   Nil.
/// </param>
/// <returns>
///   TX_RESULT_OK: The property value was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetPropertyValueAsObject(
  hProperty: TX_PROPERTYHANDLE;
  hObject: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Sets the value of a property to a blob. <br />If the property already has
///   a value that will be overwritten, regardless of type. <br />The value
///   type will be set to TX_PROPERTYVALUETYPE_BLOB. <br />
/// </summary>
/// <param name="hProperty">
///   A TX_PROPERTYHANDLE to the property for which the value should be set. <br />
///   Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="pBuffer">
///   A pointer to an array of bytes. <br />Must not be Nil. <br />
/// </param>
/// <param name="blobSize">
///   A TX_SIZE which specifies the size of the blob (i.e. the number of
///   bytes).
/// </param>
/// <returns>
///   TX_RESULT_OK: The property value was successfully set. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txSetPropertyValueAsBlob(
  hProperty: TX_PROPERTYHANDLE;
  pBuffer: PTXBYTE;
  blobSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the value of a property as a TX_INTEGER. <br />If the property does
///   not have a value of this type this call will fail.
/// </summary>
/// <param name="hProperty">
///   A TX_CONSTPROPERTYHANDLE to the property for which the value should be
///   retrieved. <br />Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="IntValue">
///   A TX_INTEGER which will be set to the value of the property.
/// </param>
/// <returns>
///   TX_RESULT_OK: The property value was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDPROPERTYTYPE: The value
///   type was not TX_PROPERTYVALUETYPE_INTEGER.
/// </returns>
function txGetPropertyValueAsInteger(
  hProperty: TX_CONSTPROPERTYHANDLE;
  var IntValue: TX_INTEGER
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the value of a property as a TX_REAL. <br />If the property does not
///   have a value or have a value of another type this call will fail. <br />
/// </summary>
/// <param name="hProperty">
///   A TX_CONSTPROPERTYHANDLE to the property for which the value should be
///   retrieved. <br />Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="RealValue">
///   A TX_REAL which will be set to the value of the property. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The property value was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDPROPERTYTYPE: The value
///   type was not TX_PROPERTYVALUETYPE_REAL.
/// </returns>
function txGetPropertyValueAsReal(
  hProperty: TX_CONSTPROPERTYHANDLE;
  var RealValue: TX_REAL
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the value of a property as a string. <br />If the property does not
///   have a value or have a value of another type this call will fail.
/// </summary>
/// <param name="hProperty">
///   A TX_CONSTPROPERTYHANDLE to the property for which the value should be
///   retrieved. <br />Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="pStringValue">
///   A TX_STRING to which the property value will be copied. <br />Must be at
///   least the size of the value. <br />Can be NULL to only get the size of
///   the value.
/// </param>
/// <param name="StringSize">
///   A TX_SIZE which will be set to the size of the property value. <br />The
///   value must be 0 if pStringValue is NULL. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The property value or the required size of the string was
///   successfully retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT:
///   An invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of pStringValue is invalid
///   (*pStringSize will be set to the required size). <br />
///   TX_RESULT_INVALIDPROPERTYTYPE: The value type was not
///   TX_PROPERTYVALUETYPE_STRING.
/// </returns>
function txGetPropertyValueAsString(
  hProperty: TX_CONSTPROPERTYHANDLE;
  pStringValue: TX_STRING;
  var StringSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the value of a property as an interaction object. <br />If the
///   property does not have a value or have a value of another type this call
///   will fail.
/// </summary>
/// <param name="hProperty">
///   A TX_CONSTPROPERTYHANDLE to the property for which the value should be
///   retrieved. <br />Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="hObject">
///   A TX_HANDLE which will be set to the value of the property. <br />This
///   handle must be released using txReleaseObject to avoid leaks. <br />The
///   value of the pointer must be set to TX_EMPTY_HANDLE. <br />
/// </param>
/// <returns>
///   TX_RESULT_OK: The property value was successfully retrieved. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function. <br />TX_RESULT_INVALIDPROPERTYTYPE: The value
///   type was not TX_PROPERTYVALUETYPE_OBJECT.
/// </returns>
function txGetPropertyValueAsObject(
  hProperty: TX_CONSTPROPERTYHANDLE;
  var hObject: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Gets the value of a property as a blob. <br />If the property does not
///   have a value or have a value of another type this call will fail.
/// </summary>
/// <param name="hProperty">
///   A TX_CONSTPROPERTYHANDLE to the property for which the value should be
///   retrieved. <br />Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="pBuffer">
///   A pointer to a byte array to which the property value will be copied. <br />
///   Must be at least the size of the value (i.e. number of bytes in the
///   blob). <br />Can be Nil to only get the size of the blob.
/// </param>
/// <param name="BlobSize">
///   <para>
///     A pointer to a TX_SIZE which will be set to the size of the blob.
///   </para>
///   <para>
///     The value must be 0 if pBuffer is NiL. <br />
///   </para>
/// </param>
/// <returns>
///   TX_RESULT_OK: The property value or the required size of the buffer was
///   successfully retrieved. <br />TX_RESULT_EYEXNOTINITIALIZED: The EyeX
///   client environment is not initialized. <br />TX_RESULT_INVALIDARGUMENT:
///   An invalid argument was passed to the function. <br />
///   TX_RESULT_INVALIDBUFFERSIZE: The size of pBuffer is invalid (*pBlobSize
///   will be set to the required size). <br />TX_RESULT_INVALIDPROPERTYTYPE:
///   The value type was not TX_PROPERTYVALUETYPE_BLOB.
/// </returns>
function txGetPropertyValueAsBlob(
  hProperty: TX_CONSTPROPERTYHANDLE;
  pBuffer: PTXBYTE;
  var BlobSize: TX_SIZE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


/// <summary>
///   Makes a shallow copy of the properties in the source object to the target
///   object. <br />
/// </summary>
/// <param name="hSourceObject">
///   A TX_CONSTHANDLE to the source object. <br />Must not be TX_EMPTY_HANDLE.
/// </param>
/// <param name="hTargetObject">
///   A TX_HANDLE to the target object. <br />Must not be TX_EMPTY_HANDLE.
/// </param>
/// <returns>
///   TX_RESULT_OK: The properties were successfully copied. <br />
///   TX_RESULT_EYEXNOTINITIALIZED: The EyeX client environment is not
///   initialized. <br />TX_RESULT_INVALIDARGUMENT: An invalid argument was
///   passed to the function.
/// </returns>
function txCopyProperties(
  hSourceObject: TX_CONSTHANDLE;
  hTargetObject: TX_HANDLE
): TX_RESULT; {$IFDEF WIN32} cdecl; {$ENDIF} {$IFDEF WIN64} stdcall; {$ENDIF}


implementation

function txInitializeEyeX; external eyeXdll name 'txInitializeEyeX';
function txUninitializeEyeX; external eyeXdll name 'txUninitializeEyeX';
function txIsEyeXInitialized; external eyeXdll name 'txIsEyeXInitialized';
function txSetInvalidArgumentHandler; external eyeXdll name 'txSetInvalidArgumentHandler';
function txEnableMonoCallbacks; external eyeXdll name 'txEnableMonoCallbacks';
function txGetEyeXAvailability; external eyeXdll name 'txGetEyeXAvailability';
function txWriteLogMessage; external eyeXdll name 'txWriteLogMessage';
function txCreateContext; external eyeXdll name 'txCreateContext';
function txReleaseContext; external eyeXdll name 'txReleaseContext';
function txShutdownContext; external eyeXdll name 'txShutdownContext';
function txSetContextName; external eyeXdll name 'txSetContextName';
function txSetContextParams; external eyeXdll name 'txSetContextParams';
function txGetContextName; external eyeXdll name 'txGetContextName';
function txGetTrackedObjects; external eyeXdll name 'txGetTrackedObjects';
function txEnableConnection; external eyeXdll name 'txEnableConnection';
function txDisableConnection; external eyeXdll name 'txDisableConnection';
function txGetConnectionState; external eyeXdll name 'txGetConnectionState';
function txRegisterConnectionStateChangedHandler; external eyeXdll name 'txRegisterConnectionStateChangedHandler';
function txUnregisterConnectionStateChangedHandler; external eyeXdll name 'txUnregisterConnectionStateChangedHandler';
function txRegisterMessageHandler; external eyeXdll name 'txRegisterMessageHandler';
function txUnregisterMessageHandler; external eyeXdll name 'txUnregisterMessageHandler';
function txRegisterQueryHandler; external eyeXdll name 'txRegisterQueryHandler';
function txUnregisterQueryHandler; external eyeXdll name 'txUnregisterQueryHandler';
function txRegisterEventHandler; external eyeXdll name 'txRegisterEventHandler';
function txUnregisterEventHandler; external eyeXdll name 'txUnregisterEventHandler';
function txPerformScheduledJobs; external eyeXdll name 'txPerformScheduledJobs';
function txGetContext; external eyeXdll name 'txGetContext';
function txGetObjectType; external eyeXdll name 'txGetObjectType';
function txGetObjectTypeName; external eyeXdll name 'txGetObjectTypeName';
function txReleaseObject; external eyeXdll name 'txReleaseObject';
function txFormatObjectAsText; external eyeXdll name 'txFormatObjectAsText';
function txGetAsyncDataResultCode; external eyeXdll name 'txGetAsyncDataResultCode';
function txGetAsyncDataContent; external eyeXdll name 'txGetAsyncDataContent';
function txCreateSnapshot; external eyeXdll name 'txCreateSnapshot';
function txCreateSnapshotWithQueryBounds; external eyeXdll name 'txCreateSnapshotWithQueryBounds';
function txCreateSnapshotForQuery; external eyeXdll name 'txCreateSnapshotForQuery';
function txCommitSnapshotAsync; external eyeXdll name 'txCommitSnapshotAsync';
function txGetSnapshotBounds; external eyeXdll name 'txGetSnapshotBounds';
function txGetSnapshotWindowIdCount; external eyeXdll name 'txGetSnapshotWindowIdCount';
function txGetSnapshotWindowId; external eyeXdll name 'txGetSnapshotWindowId';
function txAddSnapshotWindowId; external eyeXdll name 'txAddSnapshotWindowId';
function txCreateInteractor; external eyeXdll name 'txCreateInteractor';
function txCreateRectangularInteractor; external eyeXdll name 'txCreateRectangularInteractor';
function txRemoveInteractor; external eyeXdll name 'txRemoveInteractor';
function txGetInteractors; external eyeXdll name 'txGetInteractors';
function txCreateSnapshotBounds; external eyeXdll name 'txCreateSnapshotBounds';
function txDeleteSnapshotBounds; external eyeXdll name 'txDeleteSnapshotBounds';
function txCreateGlobalInteractorSnapshot; external eyeXdll name 'txCreateGlobalInteractorSnapshot';
function txGetBoundsType; external eyeXdll name 'txGetBoundsType';
function txSetRectangularBoundsData; external eyeXdll name 'txSetRectangularBoundsData';
function txSetRectangularBoundsDataRect; external eyeXdll name 'txSetRectangularBoundsDataRect';
function txGetRectangularBoundsData; external eyeXdll name 'txGetRectangularBoundsData';
function txGetRectangularBoundsDataRect; external eyeXdll name 'txGetRectangularBoundsDataRect';
function txBoundsIntersect; external eyeXdll name 'txBoundsIntersect';
function txBoundsIntersectRect; external eyeXdll name 'txBoundsIntersectRect';
function txGetInteractorId; external eyeXdll name 'txGetInteractorId';
function txGetInteractorParentId; external eyeXdll name 'txGetInteractorParentId';
function txGetInteractorWindowId; external eyeXdll name 'txGetInteractorWindowId';
function txSetInteractorZ; external eyeXdll name 'txSetInteractorZ';
function txGetInteractorZ; external eyeXdll name 'txGetInteractorZ';
function txSetInteractorEnabled; external eyeXdll name 'txSetInteractorEnabled';
function txGetInteractorEnabled; external eyeXdll name 'txGetInteractorEnabled';
function txSetInteractorDeleted; external eyeXdll name 'txSetInteractorEnabled';
function txGetInteractorDeleted; external eyeXdll name 'txGetInteractorEnabled';
function txCreateInteractorBounds; external eyeXdll name 'txCreateInteractorBounds';
function txDeleteInteractorBounds; external eyeXdll name 'txDeleteInteractorBounds';
function txGetInteractorBounds; external eyeXdll name 'txGetInteractorBounds';
function txCreateInteractorBehavior; external eyeXdll name 'txCreateInteractorBehavior';
function txRemoveInteractorBehavior; external eyeXdll name 'txRemoveInteractorBehavior';
function txGetInteractorBehavior; external eyeXdll name 'txGetInteractorBehavior';
function txGetInteractorBehaviors; external eyeXdll name 'txGetInteractorBehaviors';
function txCreateGazeAwareBehavior; external eyeXdll name 'txCreateGazeAwareBehavior';
function txCreateActivatableBehavior; external eyeXdll name 'txCreateActivatableBehavior';
function txCreatePannableBehavior; external eyeXdll name 'txCreatePannableBehavior';
function txCreateGazePointDataBehavior; external eyeXdll name 'txCreateGazePointDataBehavior';
function txCreateFixationDataBehavior; external eyeXdll name 'txCreateFixationDataBehavior';
function txCreateMask; external eyeXdll name 'txCreateMask';
function txRemoveMask; external eyeXdll name 'txRemoveMask';
function txGetMask; external eyeXdll name 'txGetMask';
function txGetMaskData; external eyeXdll name 'txGetMaskData';
function txSetMaskBounds; external eyeXdll name 'txSetMaskBounds';
function txClearMaskBounds; external eyeXdll name 'txClearMaskBounds';
function txGetMaskBounds; external eyeXdll name 'txGetMaskBounds';
function txCreateCommand; external eyeXdll name 'txCreateCommand';
function txGetCommandType; external eyeXdll name 'txGetCommandType';
function txSetCommandData; external eyeXdll name 'txSetCommandData';
function txGetCommandData; external eyeXdll name 'txGetCommandData';
function txExecuteCommandAsync; external eyeXdll name 'txExecuteCommandAsync';
function txCreateActionCommand; external eyeXdll name 'txCreateActionCommand';
function txDisableBuiltinKeys; external eyeXdll name 'txDisableBuiltinKeys';
function txEnableBuiltinKeys; external eyeXdll name 'txEnableBuiltinKeys';
function txLaunchConfigurationTool; external eyeXdll name 'txLaunchConfigurationTool';
function txSetCurrentProfile; external eyeXdll name 'txSetCurrentProfile';
function txDeleteProfile; external eyeXdll name 'txDeleteProfile';
function txGetBehaviorType; external eyeXdll name 'txGetBehaviorType';
function txSetActivatableBehaviorParams; external eyeXdll name 'txSetActivatableBehaviorParams';
function txGetActivatableBehaviorParams; external eyeXdll name 'txGetActivatableBehaviorParams';
function txGetActivatableEventType; external eyeXdll name 'txGetActivatableEventType';
function txGetActivationFocusChangedEventParams; external eyeXdll name 'txGetActivationFocusChangedEventParams';
function txSetPannableBehaviorParams; external eyeXdll name 'txSetPannableBehaviorParams';
function txGetPannableBehaviorParams; external eyeXdll name 'txGetPannableBehaviorParams';
function txGetPannableEventType; external eyeXdll name 'txGetPannableEventType';
function txGetPannablePanEventParams; external eyeXdll name 'txGetPannablePanEventParams';
function txGetPannableStepEventParams; external eyeXdll name 'txGetPannableStepEventParams';
function txGetPannableHandsFreeEventParams; external eyeXdll name 'txGetPannableHandsFreeEventParams';
function txSetGazePointDataBehaviorParams; external eyeXdll name 'txSetGazePointDataBehaviorParams';
function txGetGazePointDataBehaviorParams; external eyeXdll name 'txGetGazePointDataBehaviorParams';
function txGetGazePointDataEventParams; external eyeXdll name 'txGetGazePointDataEventParams';
function txSetGazeAwareBehaviorParams; external eyeXdll name 'txSetGazeAwareBehaviorParams';
function txGetGazeAwareBehaviorParams; external eyeXdll name 'txGetGazeAwareBehaviorParams';
function txGetGazeAwareBehaviorEventParams; external eyeXdll name 'txGetGazeAwareBehaviorEventParams';
function txSetFixationDataBehaviorParams; external eyeXdll name 'txSetFixationDataBehaviorParams';
function txGetFixationDataBehaviorParams; external eyeXdll name 'txGetFixationDataBehaviorParams';
function txGetFixationDataEventParams; external eyeXdll name 'txGetFixationDataEventParams';
function txGetEyePositionDataEventParams; external eyeXdll name 'txGetEyePositionDataEventParams';
function txGetBehaviorEventTimestamp; external eyeXdll name 'txGetBehaviorEventTimestamp';
function txGetStateAsync; external eyeXdll name 'txGetStateAsync';
function txGetState; external eyeXdll name 'txGetState';
function txSetStateAsync; external eyeXdll name 'txSetStateAsync';
function txCreateStateBag; external eyeXdll name 'txCreateStateBag';
function txGetStateBagPath; external eyeXdll name 'txGetStateBagPath';
function txRegisterStateObserver; external eyeXdll name 'txRegisterStateObserver';
function txUnregisterStateObserver; external eyeXdll name 'txUnregisterStateObserver';
function txGetStateValueAsInteger; external eyeXdll name 'txGetStateValueAsInteger';
function txGetStateValueAsReal; external eyeXdll name 'txGetStateValueAsReal';
function txGetStateValueAsString; external eyeXdll name 'txGetStateValueAsString';
function txGetStateValueAsRectangle; external eyeXdll name 'txGetStateValueAsRectangle';
function txGetStateValueAsVector2; external eyeXdll name 'txGetStateValueAsVector2';
function txGetStateValueAsSize2; external eyeXdll name 'txGetStateValueAsSize2';
function txSetStateValueAsInteger; external eyeXdll name 'txSetStateValueAsInteger';
function txSetStateValueAsReal; external eyeXdll name 'txSetStateValueAsReal';
function txSetStateValueAsString; external eyeXdll name 'txSetStateValueAsString';
function txSetStateValueAsRectangle; external eyeXdll name 'txSetStateValueAsRectangle';
function txSetStateValueAsVector2; external eyeXdll name 'txSetStateValueAsVector2';
function txSetStateValueAsSize2; external eyeXdll name 'txSetStateValueAsSize2';
function txGetPropertyForStateValue; external eyeXdll name 'txGetPropertyForStateValue';
function txRegisterStateChangedHandler; external eyeXdll name 'txRegisterStateChangedHandler';
function txUnregisterStateChangedHandler; external eyeXdll name 'txUnregisterStateChangedHandler';
function txGetNotificationType; external eyeXdll name 'txGetNotificationType';
function txGetNotificationData; external eyeXdll name 'txGetNotificationData';
function txGetQueryBounds; external eyeXdll name 'txGetQueryBounds';
function txGetQueryWindowIdCount; external eyeXdll name 'txGetQueryWindowIdCount';
function txGetQueryWindowId; external eyeXdll name 'txGetQueryWindowId';
function txGetEventInteractorId; external eyeXdll name 'txGetEventInteractorId';
function txGetEventBehavior; external eyeXdll name 'txGetEventBehavior';
function txGetEventBehaviors; external eyeXdll name 'txGetEventBehaviors';
function txCreatePropertyBag; external eyeXdll name 'txCreatePropertyBag';
function txGetPropertyBagType; external eyeXdll name 'txGetPropertyBagType';
function txCreateProperty; external eyeXdll name 'txCreateProperty';
function txRemoveProperty; external eyeXdll name 'txRemoveProperty';
function txGetProperty; external eyeXdll name 'txGetProperty';
function txGetProperties; external eyeXdll name 'txGetProperties';
function txGetPropertyName; external eyeXdll name 'txGetPropertyName';
function txGetPropertyValueType; external eyeXdll name 'txGetPropertyValueType';
function txGetPropertyFlags; external eyeXdll name 'txGetPropertyFlags';
function txClearPropertyValue; external eyeXdll name 'txClearPropertyValue';
function txSetPropertyValueAsInteger; external eyeXdll name 'txSetPropertyValueAsInteger';
function txSetPropertyValueAsReal; external eyeXdll name 'txSetPropertyValueAsReal';
function txSetPropertyValueAsString; external eyeXdll name 'txSetPropertyValueAsString';
function txSetPropertyValueAsObject; external eyeXdll name 'txSetPropertyValueAsObject';
function txSetPropertyValueAsBlob; external eyeXdll name 'txSetPropertyValueAsBlob';
function txGetPropertyValueAsInteger; external eyeXdll name 'txGetPropertyValueAsInteger';
function txGetPropertyValueAsReal; external eyeXdll name 'txGetPropertyValueAsReal';
function txGetPropertyValueAsString; external eyeXdll name 'txGetPropertyValueAsString';
function txGetPropertyValueAsObject; external eyeXdll name 'txGetPropertyValueAsObject';
function txGetPropertyValueAsBlob; external eyeXdll name 'txGetPropertyValueAsBlob';
function txCopyProperties; external eyeXdll name 'txCopyProperties';


end.
