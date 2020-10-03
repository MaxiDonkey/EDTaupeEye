{*******************************************************}
{                                                       }
{             02/2020  MaxiDonkey  Library              }
{                                                       }
{*******************************************************}

unit KeysDef;

interface
uses
  SysUtils, classes, Windows, Messages, Dialogs, Forms, StrUtils, uRegistry,
  ShellApi, uProcessusManagment;

type
  TSpecialShift = (ss_lshift, ss_rshift,
                   ss_lctrl,  ss_rctrl,
                   ss_lalt,   ss_ralt);
  TSpecials     = set of TSpecialShift;

  TKeyKind     = (kk_touche, kk_codetouche, kk_function, kk_keyfx, kk_keyalpha,
                  kk_keydir, kk_winapp,     kk_elite,    kk_timer);

  TKeyTimer    = (
     kt_none
  );

  TKeyElite    = (
     ke_none
  );

  TKeyWinApp   = (
     kw_none,
     kx_win,       kx_wind,       kx_winm,        kx_wine,        kx_winl,
     kx_winr,      kx_winp,       kx_winf,        kx_win1,        kx_win2,
     kx_win3,      kx_win4,       kx_win5,        kx_win6,        kx_win7,
     kx_win8,      kx_win9,       kx_win0,        kx_minimize,    kx_restore,
     kx_close,
     kx_medplay,   kx_medstop,    kx_mednext,     kx_medprior,
     kx_volmute,   kx_voldown,    kx_volup,
     kx_navback,   kx_navforward, kx_navrefresh,  kx_navstop,     kx_navsearch,
     kx_navfavor,  kx_navhome,    kx_navgolink,   kx_navlnext,    kx_navlprior,
     kx_navfull,   kx_sheetclose, kx_sheetnew,    kx_sheetnext,   kx_sheetprior,
     kx_navnew,    kx_navzoomin,  kx_navzoomout,  kx_navnozoom,
     {Mouse event}
     kx_left_clic, kx_right_clic, kx_middle_clic, kx_double_clic,
     kx_notepad
  );

  TKeyIdent    =
    (ki_none,

     {no upper distinction}
     ki_quot,      ki_dquot,     ki_pcent,     ki_plus,        ki_moins,
     ki_slash,     ki_aster,     ki_equal,     ki_diese,       ki_dolar,
     ki_livre,     ki_yen,       ki_excla,     ki_intero,      ki_virgul,
     ki_point,     ki_point2,    ki_semic,     ki_arobas,      ki_chapo,
     ki_antislash, ki_degre,     ki_paragrf,   ki_mu,          ki_underscore,
     ki_pargauch,  ki_pardroit,  ki_bragauch,  ki_bradroit,    ki_accgauch,
     ki_accdroit,  ki_etcomerc,  ki_infa,      ki_supa,        ki_pipe,
     ki_cube,      ki_puis1,     ki_trema,     ki_guigauch,    ki_guidroit,
     ki_divise,    ki_trademark, ki_pi,        ki_multipli,    ki_copyright,
     ki_plusmoins, ki_macron,    ki_1demi,     ki_1quart,      ki_3quart,
     ki_tilde,     ki_function,

     {upper distinction}
     ki_agrav,     ki_acircon,   ki_atrema,
     ki_ccedil,
     ki_egrav,     ki_eaigu,     ki_ecircon,   ki_etrema,
     ki_icircon,   ki_itrema,
     ki_ocircon,
     ki_ugrav,     ki_ucircon,
     ki_aelie,     ki_oelie,
     ki_letempty,  ki_angstroem, ki_nenj
    );

  TKeyEditor = (
     kf_none,               kf_selectall,        kf_copy,             kf_paste,          kf_cut,
     kf_unselect,           kf_clear,            kf_home,             kf_end,            kf_tophome,
     kf_bottomend,          kf_seltoend,         kf_selalltoend,      kf_seltohome,      kf_selalltohome,
     kf_selhomepage,        kf_selendpage,       kf_penon,            kf_penoff,         kf_selword,
     kf_wordnext,           kf_wordprior,        kf_alt,              kf_search,         kf_selsearch,
     kf_searchnext,         kf_menuopen,         kf_menuclose,        kf_ctrl,           kf_altrelease,
     kf_activsoft,          kf_again,            kf_again2,           kf_again3,         kf_again5   
  );

  TKeydir = (
     kd_none,
     kd_left,               kd_right,            kd_up,               kd_down,           kd_home,
     kd_end,                kd_esc,              kd_return,           kd_tab,            kd_pagenext,
     kd_pageprior,          kd_space,            kd_backspace,        kd_suppr,          kd_insert,
     kd_capital,            kd_numlock,
     kd_shift_left,         kd_shift_right,      kd_shift_up,         kd_shift_down,     kd_shift_home,
     kd_shift_end,          kd_shift_esc,        kd_shift_return,     kd_shift_tab,      kd_shift_pagenext,
     kd_shift_pageprior,    kd_shift_space,      kd_shift_backspace,  kd_shift_suppr,    kd_shift_insert,
     kd_shift_capital,      kd_shift_numlock,
     kd_ctrl_left,          kd_ctrl_right,       kd_ctrl_up,          kd_ctrl_down,      kd_ctrl_home,
     kd_ctrl_end,           kd_ctrl_esc,         kd_ctrl_return,      kd_ctrl_tab,       kd_ctrl_pagenext,
     kd_ctrl_pageprior,     kd_ctrl_space,       kd_ctrl_backspace,   kd_ctrl_suppr,     kd_ctrl_insert,
     kd_ctrl_capital,       kd_ctrl_numlock,
     kd_alt_left,           kd_alt_right,        kd_alt_up,           kd_alt_down,       kd_alt_home,
     kd_alt_end,            kd_alt_esc,          kd_alt_return,       kd_alt_tab,        kd_alt_pagenext,
     kd_alt_pageprior,      kd_alt_space,        kd_alt_backspace,    kd_alt_suppr,      kd_alt_insert,
     kd_alt_capital,        kd_alt_numlock,
     kd_shctrl_left,        kd_shctrl_right,     kd_shctrl_up,        kd_shctrl_down,    kd_shctrl_home,
     kd_shctrl_end,         kd_shctrl_esc,       kd_shctrl_return,    kd_shctrl_tab,     kd_shctrl_pagenext,
     kd_shctrl_pageprior,   kd_shctrl_space,     kd_shctrl_backspace, kd_shctrl_suppr,   kd_shctrl_insert,
     kd_shctrl_capital,     kd_shctrl_numlock,
     kd_shalt_left,         kd_shalt_right,      kd_shalt_up,         kd_shalt_down,     kd_shalt_home,
     kd_shalt_end,          kd_shalt_esc,        kd_shalt_return,     kd_shalt_tab,      kd_shalt_pagenext,
     kd_shalt_pageprior,    kd_shalt_space,      kd_shalt_backspace,  kd_shalt_suppr,    kd_shalt_insert,
     kd_shalt_capital,      kd_shalt_numlock,
     kd_alctrl_left,        kd_alctrl_right,     kd_alctrl_up,        kd_alctrl_down,    kd_alctrl_home,
     kd_alctrl_end,         kd_alctrl_esc,       kd_alctrl_return,    kd_alctrl_tab,     kd_alctrl_pagenext,
     kd_alctrl_pageprior,   kd_alctrl_space,     kd_alctrl_backspace, kd_alctrl_suppr,   kd_alctrl_insert,
     kd_alctrl_capital,     kd_alctrl_numlock,
     {non implémentés vocalement}
     kd_sac_left,           kd_sac_right,        kd_sac_up,           kd_sac_down,       kd_sac_home,
     kd_sac_end,            kd_sac_esc,          kd_sac_return,       kd_sac_tab,        kd_sac_pagenext,
     kd_sac_pageprior,      kd_sac_space,        kd_sac_backspace,    kd_sac_suppr,      kd_sac_insert,
     kd_sac_capital,        kd_sac_numlock
  );

  TKeyFx = (
     kx_none,         kx_f1,         kx_f2,
     kx_f3,           kx_f4,         kx_f5,         kx_f6,           kx_f7,
     kx_f8,           kx_f9,         kx_f10,        kx_f11,          kx_f12,
     kx_f13,          kx_f14,        kx_f15,        kx_f16,          kx_f17,
     kx_f18,          kx_f19,        kx_f20,        kx_f21,          kx_f22,
     kx_f23,          kx_f24,        kx_shift_f1,   kx_shift_f2,     kx_shift_f3,
     kx_shift_f4,     kx_shift_f5,   kx_shift_f6,   kx_shift_f7,     kx_shift_f8,
     kx_shift_f9,     kx_shift_f10,  kx_shift_f11,  kx_shift_f12,    kx_shift_f13,
     kx_shift_f14,    kx_shift_f15,  kx_shift_f16,  kx_shift_f17,    kx_shift_f18,
     kx_shift_f19,    kx_shift_f20,  kx_shift_f21,  kx_shift_f22,    kx_shift_f23,
     kx_shift_f24,    kx_alt_f1,     kx_alt_f2,     kx_alt_f3,       kx_alt_f4,
     kx_alt_f5,       kx_alt_f6,     kx_alt_f7,     kx_alt_f8,       kx_alt_f9,
     kx_alt_f10,      kx_alt_f11,    kx_alt_f12,    kx_alt_f13,      kx_alt_f14,
     kx_alt_f15,      kx_alt_f16,    kx_alt_f17,    kx_alt_f18,      kx_alt_f19,
     kx_alt_f20,      kx_alt_f21,    kx_alt_f22,    kx_alt_f23,      kx_alt_f24,
     kx_ctrl_f1,      kx_ctrl_f2,    kx_ctrl_f3,    kx_ctrl_f4,      kx_ctrl_f5,
     kx_ctrl_f6,      kx_ctrl_f7,    kx_ctrl_f8,    kx_ctrl_f9,      kx_ctrl_f10,
     kx_ctrl_f11,     kx_ctrl_f12,   kx_ctrl_f13,   kx_ctrl_f14,     kx_ctrl_f15,
     kx_ctrl_f16,     kx_ctrl_f17,   kx_ctrl_f18,   kx_ctrl_f19,     kx_ctrl_f20,
     kx_ctrl_f21,     kx_ctrl_f22,   kx_ctrl_f23,   kx_ctrl_f24,     kx_shalt_f1,
     kx_shalt_f2,     kx_shalt_f3,   kx_shalt_f4,   kx_shalt_f5,     kx_shalt_f6,
     kx_shalt_f7,     kx_shalt_f8,   kx_shalt_f9,   kx_shalt_f10,    kx_shalt_f11,
     kx_shalt_f12,    kx_shalt_f13,  kx_shalt_f14,  kx_shalt_f15,    kx_shalt_f16,
     kx_shalt_f17,    kx_shalt_f18,  kx_shalt_f19,  kx_shalt_f20,    kx_shalt_f21,
     kx_shalt_f22,    kx_shalt_f23,  kx_shalt_f24,  kx_ctalt_f1,     kx_ctalt_f2,
     kx_ctalt_f3,     kx_ctalt_f4,   kx_ctalt_f5,   kx_ctalt_f6,     kx_ctalt_f7,
     kx_ctalt_f8,     kx_ctalt_f9,   kx_ctalt_f10,  kx_ctalt_f11,    kx_ctalt_f12,
     kx_ctalt_f13,    kx_ctalt_f14,  kx_ctalt_f15,  kx_ctalt_f16,    kx_ctalt_f17,
     kx_ctalt_f18,    kx_ctalt_f19,  kx_ctalt_f20,  kx_ctalt_f21,    kx_ctalt_f22,
     kx_ctalt_f23,    kx_ctalt_f24,  kx_shctrl_f1,  kx_shctrl_f2,    kx_shctrl_f3,
     kx_shctrl_f4,    kx_shctrl_f5,  kx_shctrl_f6,  kx_shctrl_f7,    kx_shctrl_f8,
     kx_shctrl_f9,    kx_shctrl_f10, kx_shctrl_f11, kx_shctrl_f12,   kx_shctrl_f13,
     kx_shctrl_f14,   kx_shctrl_f15, kx_shctrl_f16, kx_shctrl_f17,   kx_shctrl_f18,
     kx_shctrl_f19,   kx_shctrl_f20, kx_shctrl_f21, kx_shctrl_f22,   kx_shctrl_f23,
     kx_shctrl_f24,
     {non implémentés vocalement}
     kx_sac_f1,       kx_sac_f2,     kx_sac_f3,
     kx_sac_f4,       kx_sac_f5,     kx_sac_f6,     kx_sac_f7,       kx_sac_f8,
     kx_sac_f9,       kx_sac_f10,    kx_sac_f11,    kx_sac_f12,      kx_sac_f13,
     kx_sac_f14,      kx_sac_f15,    kx_sac_f16,    kx_sac_f17,      kx_sac_f18,
     kx_sac_f19,      kx_sac_f20,    kx_sac_f21,    kx_sac_f22,      kx_sac_f23,
     kx_sac_f24
  );

  TKeyAlpha = (
     ka_none,
     ka_ctrl_a,       ka_ctrl_b,       ka_ctrl_c,
     ka_ctrl_d,       ka_ctrl_e,       ka_ctrl_f,       ka_ctrl_g,         ka_ctrl_h,
     ka_ctrl_i,       ka_ctrl_j,       ka_ctrl_k,       ka_ctrl_l,         ka_ctrl_m,
     ka_ctrl_n,       ka_ctrl_o,       ka_ctrl_p,       ka_ctrl_q,         ka_ctrl_r,
     ka_ctrl_s,       ka_ctrl_t,       ka_ctrl_u,       ka_ctrl_v,         ka_ctrl_w,
     ka_ctrl_x,       ka_ctrl_y,       ka_ctrl_z,
     ka_shift_a,      ka_shift_b,      ka_shift_c,
     ka_shift_d,      ka_shift_e,      ka_shift_f,      ka_shift_g,        ka_shift_h,
     ka_shift_i,      ka_shift_j,      ka_shift_k,      ka_shift_l,        ka_shift_m,
     ka_shift_n,      ka_shift_o,      ka_shift_p,      ka_shift_q,        ka_shift_r,
     ka_shift_s,      ka_shift_t,      ka_shift_u,      ka_shift_v,        ka_shift_w,
     ka_shift_x,      ka_shift_y,      ka_shift_z,
     ka_alt_a,        ka_alt_b,        ka_alt_c,
     ka_alt_d,        ka_alt_e,        ka_alt_f,        ka_alt_g,          ka_alt_h,
     ka_alt_i,        ka_alt_j,        ka_alt_k,        ka_alt_l,          ka_alt_m,
     ka_alt_n,        ka_alt_o,        ka_alt_p,        ka_alt_q,          ka_alt_r,
     ka_alt_s,        ka_alt_t,        ka_alt_u,        ka_alt_v,          ka_alt_w,
     ka_alt_x,        ka_alt_y,        ka_alt_z,
     ka_alt_ctrl_a,   ka_alt_ctrl_b,   ka_alt_ctrl_c,
     ka_alt_ctrl_d,   ka_alt_ctrl_e,   ka_alt_ctrl_f,   ka_alt_ctrl_g,     ka_alt_ctrl_h,
     ka_alt_ctrl_i,   ka_alt_ctrl_j,   ka_alt_ctrl_k,   ka_alt_ctrl_l,     ka_alt_ctrl_m,
     ka_alt_ctrl_n,   ka_alt_ctrl_o,   ka_alt_ctrl_p,   ka_alt_ctrl_q,     ka_alt_ctrl_r,
     ka_alt_ctrl_s,   ka_alt_ctrl_t,   ka_alt_ctrl_u,   ka_alt_ctrl_v,     ka_alt_ctrl_w,
     ka_alt_ctrl_x,   ka_alt_ctrl_y,   ka_alt_ctrl_z,
     ka_shift_ctrl_a, ka_shift_ctrl_b, ka_shift_ctrl_c,
     ka_shift_ctrl_d, ka_shift_ctrl_e, ka_shift_ctrl_f, ka_shift_ctrl_g,   ka_shift_ctrl_h,
     ka_shift_ctrl_i, ka_shift_ctrl_j, ka_shift_ctrl_k, ka_shift_ctrl_l,   ka_shift_ctrl_m,
     ka_shift_ctrl_n, ka_shift_ctrl_o, ka_shift_ctrl_p, ka_shift_ctrl_q,   ka_shift_ctrl_r,
     ka_shift_ctrl_s, ka_shift_ctrl_t, ka_shift_ctrl_u, ka_shift_ctrl_v,   ka_shift_ctrl_w,
     ka_shift_ctrl_x, ka_shift_ctrl_y, ka_shift_ctrl_z,
     ka_shift_alt_a,  ka_shift_alt_b,  ka_shift_alt_c,
     ka_shift_alt_d,  ka_shift_alt_e,  ka_shift_alt_f,  ka_shift_alt_g,    ka_shift_alt_h,
     ka_shift_alt_i,  ka_shift_alt_j,  ka_shift_alt_k,  ka_shift_alt_l,    ka_shift_alt_m,
     ka_shift_alt_n,  ka_shift_alt_o,  ka_shift_alt_p,  ka_shift_alt_q,    ka_shift_alt_r,
     ka_shift_alt_s,  ka_shift_alt_t,  ka_shift_alt_u,  ka_shift_alt_v,    ka_shift_alt_w,
     ka_shift_alt_x,  ka_shift_alt_y,  ka_shift_alt_z,
     {non implémentés vocalement}
     ka_sac_a,        ka_sac_b,        ka_sac_c,
     ka_sac_d,        ka_sac_e,        ka_sac_f,        ka_sac_g,          ka_sac_h,
     ka_sac_i,        ka_sac_j,        ka_sac_k,        ka_sac_l,          ka_sac_m,
     ka_sac_n,        ka_sac_o,        ka_sac_p,        ka_sac_q,          ka_sac_r,
     ka_sac_s,        ka_sac_t,        ka_sac_u,        ka_sac_v,          ka_sac_w,
     ka_sac_x,        ka_sac_y,        ka_sac_z,
     {Numeriques}
     ka_0,            ka_1,            ka_2,            ka_3,              ka_4,
     ka_5,            ka_6,            ka_7,            ka_8,              ka_9,
     {Shift+Num}
     ka_shift_0,      ka_shift_1,      ka_shift_2,      ka_shift_3,        ka_shift_4,
     ka_shift_5,      ka_shift_6,      ka_shift_7,      ka_shift_8,        ka_shift_9,
     {Alt+Num}
     ka_alt_0,        ka_alt_1,        ka_alt_2,        ka_alt_3,          ka_alt_4,
     ka_alt_5,        ka_alt_6,        ka_alt_7,        ka_alt_8,          ka_alt_9,
     {Ctrl+Num}
     ka_ctrl_0,       ka_ctrl_1,       ka_ctrl_2,       ka_ctrl_3,         ka_ctrl_4,
     ka_ctrl_5,       ka_ctrl_6,       ka_ctrl_7,       ka_ctrl_8,         ka_ctrl_9,
     {Shift+Alt+Num}
     ka_shift_alt_0,  ka_shift_alt_1,  ka_shift_alt_2,  ka_shift_alt_3,    ka_shift_alt_4,
     ka_shift_alt_5,  ka_shift_alt_6,  ka_shift_alt_7,  ka_shift_alt_8,    ka_shift_alt_9,
     {Shift+Ctrl+Num}
     ka_shift_ctrl_0, ka_shift_ctrl_1, ka_shift_ctrl_2, ka_shift_ctrl_3,   ka_shift_ctrl_4,
     ka_shift_ctrl_5, ka_shift_ctrl_6, ka_shift_ctrl_7, ka_shift_ctrl_8,   ka_shift_ctrl_9,
     {Alt+Ctrl+Num}
     ka_alt_ctrl_0,   ka_alt_ctrl_1,   ka_alt_ctrl_2,   ka_alt_ctrl_3,     ka_alt_ctrl_4,
     ka_alt_ctrl_5,   ka_alt_ctrl_6,   ka_alt_ctrl_7,   ka_alt_ctrl_8,     ka_alt_ctrl_9,
     {Shift+Alt+Ctrl+Num}
     ka_sac_0,        ka_sac_1,        ka_sac_2,        ka_sac_3,          ka_sac_4,
     ka_sac_5,        ka_sac_6,        ka_sac_7,        ka_sac_8,          ka_sac_9
  );


function StrKeyToCode(const Value: string): string;

function KeyIdentifier(const Value: string): TKeyKind;

procedure WriteLastCommand(const ASt: string);
function  ReadLastCommand:string;

{ --- ALLOWED --- }
function AllowedAgain(const Command: string):Boolean;

{ --- Current Charkey command --- }
procedure CurrentKeyCmdToReg(const Value: string);
function  CurrentKeyFromReg:string;
function  IsPenOn: Boolean;


{...............................................................................
      Classe pour la manipulation des raccorcis clavier
...............................................................................}

type
  TShortcuts = class
  private
    function  GetText: string;
  private
    procedure Execute_(const Value: string);
    property  Text_: string read GetText;
  public
    constructor Create;
    destructor Destroy; override;
    class procedure Initialize;
    class procedure Finalize;
    class procedure Execute(const Value: string);
    class function  Text: string;
    class function  EditorShortCut: string;
    class function  FxShortCut: string;
    class function  AlphaShortCut: string;
    class function  DirShortCut: string;
    class function  WinAppShortCut: string;
    class function  EliteShortCut: string;
    class function  TimerShortCut: string;
  end;


{...............................................................................
      Classe pour la manipulation vocale d'un éditeur ou d'un applicatif
      use TKeyEditors
...............................................................................}

type
  TCustomKeyEditors = class(TComponent)
  private
    FCurrent: TKeyEditor;
    function  GetText: string;
  private
    function  Enabled: Boolean;
    procedure SetCurrent(const Value: string);
    {main validations}
    procedure PenOn;
    procedure PenOff;
    procedure Alt;
    procedure Ctrl;
    procedure AltRelease;
    procedure AltTab;
    procedure DoAgain; overload;
    procedure DoAgain(const Count: Integer); overload;
    {main commands}
    procedure SelectAll;
    procedure Copy;
    procedure Paste;
    procedure Cut;
    procedure UnSelect;
    procedure Clear;
    procedure ToRight;
    procedure ToLeft;
    procedure ToHome;
    procedure ToEnd;
    procedure ToTopHome;
    procedure ToBottomEnd;
    procedure SelToEnd;
    procedure SelAllToEnd;
    procedure SelToHome;
    procedure SelAllToHome;
    procedure SelHomePage;
    procedure SelEndPage;
    procedure SelWord;
    procedure WordNext;
    procedure WordPrior;
    {Search box}
    procedure Search;
    procedure SelSearch;
    procedure SearchNext;
    {App menu}
    procedure MenuOpen;
    procedure MenuClose;
  public
    procedure Execute_(const Value: string);
    property  Text_: string read GetText;
  end;

  TKeyEditors = class(TCustomKeyEditors)
  public
    class function Initialize: TKeyEditors;
    class procedure Finalize;
    class procedure Execute(const Value: string);
    class function  Writable:Boolean;
    class function  Text:string;
  end;

  TCustomKeyFx = class(TComponent)
  private
    FCurrent: TKeyFx;
    procedure SetCurrent(const Value: string);
    procedure FKeys(const Value: TKeyFx; Specials: TSpecials = []);
    function  GetText: string;
  public
    procedure Execute_(const Value: string);
    property  Text_: string read GetText;
  end;

  TKeyBoardFx = class(TCustomKeyFx)
  public
    class function Initialize: TKeyBoardFx;
    class procedure Finalize;
    class procedure Execute(const Value: string);
    class function  Writable:Boolean;
    class function  Text:string;
  end;

  TCustomKeyAlpha = class(TComponent)
  private
    FCurrent: TKeyAlpha;
    procedure SetCurrent(const Value: string);
    function  Ctrl_char(const Value: TKeyAlpha):Char;
    function  Shift_char(const Value: TKeyAlpha):Char;
    function  Alt_char(const Value: TKeyAlpha):Char;
    function  Alt_Ctlr_char(const Value: TKeyAlpha):Char;
    function  Shift_Ctlr_char(const Value: TKeyAlpha):Char;
    function  Shift_Alt_char(const Value: TKeyAlpha):Char;
    function  Shift_Alt_Ctrl_char(const Value: TKeyAlpha):Char;
    procedure SendNumKey(const AKey: TKeyAlpha; ASpecials : TSpecials); overload;
    function  SendNumKey(const AKey, Ref: TKeyAlpha; ASpecials: TSpecials):TKeyAlpha; overload;
    function  GetText: string;
  public
    procedure Execute_(const Value: string);
    property  Text_: string read GetText;
  end;

  TKeyBoardAlpha = class(TCustomKeyAlpha)
  public
    class function  Initialize: TKeyBoardAlpha;
    class procedure Finalize;
    class procedure Execute(const Value: string);
    class function  Writable:Boolean;
    class function  Text:string;
  end;

  TCustomKeyDir = class(TComponent)
  private
    FCurrent: TKeydir;
    procedure SetCurrent(const Value: string);
    function  ModuloCode(var Specs: TSpecials):TKeydir;
    function  GetText: string;
  public
    procedure Execute_(const Value: string);
    property  Text_: string read GetText;
  end;

  TKeyBoardDir = class(TCustomKeyDir)
  public
    class function  Initialize: TKeyBoardDir;
    class procedure Finalize;
    class procedure Execute(const Value: string);
    class function  Writable:Boolean;
    class function  Text:string;
  end;

  TCustomKeyWinApp = class(TComponent)
  private
    FCurrent: TKeyWinApp;
    procedure SetCurrent(const Value: string);
    function  GetText: string;
  private
    procedure GoFirstLing;
  public
    procedure Execute_(const Value: string);
    property  Text_: string read GetText;
  end;

  TKeyBoardWinApp = class(TCustomKeyWinApp)
  public
    class function  Initialize: TKeyBoardWinApp;
    class procedure Finalize;
    class procedure Execute(const Value: string);
    class function  Text:string;
  end;

  TCustomKeyElite = class(TComponent)
  private
    FCurrent: TKeyElite;
    procedure SetCurrent(const Value: string);
    function  GetText: string;
  public
    procedure Execute_(const Value: string);
    property  Text_: string read GetText;
  end;

  TKeyBoardElite = class(TCustomKeyElite)
  public
    class function  Initialize: TKeyBoardElite;
    class procedure Finalize;
    class procedure Execute(const Value: string);
    class function  Text:string;
  end;

  TCustomKeyTimer = class(TComponent)
  private
    FCurrent: TKeyTimer;
    procedure SetCurrent(const Value: string);
    function  GetText: string;
  public
    procedure Execute_(const Value: string);
    property  Text_: string read GetText;
  end;

  TKeyBoardTimer = class(TCustomKeyTimer)
  public
    class function  Initialize: TKeyBoardTimer;
    class procedure Finalize;
    class procedure Execute(const Value: string);
    class function  Text:string;
  end;

var
  Shortcuts        : TShortcuts;

  KeyEditors       : TKeyEditors;
  KeyBoardFx       : TKeyBoardFx;
  KeyBoardAlpha    : TKeyBoardAlpha;
  KeyBoardDir      : TKeyBoardDir;
  KeyBoardWinApp   : TKeyBoardWinApp;
  KeyBoardElite    : TKeyBoardElite;
  KeyBoardTimer    : TKeyBoardTimer;

                        
implementation

uses
  SendKey32;

type
  TKeyRecord = record
    Car : Char;
    LowerCode : string;
    UpperCode : string;
  end;

  TStrKeyIdent    = array[0..Integer( High(TKeyIdent) )]    of string;
  TKeyCodes       = array[0..Integer( High(TKeyIdent) )]    of TKeyRecord;
  TStrKeyEditor   = array[0..Integer( High(TKeyEditor) )]   of string;
  TStrKeyFx       = array[0..Integer( High(TKeyFx) )]       of string;
  TStrKeyAlpha    = array[0..Integer( High(TKeyAlpha) )]    of string;
  TStrKeyDir      = array[0..Integer( High(TKeyDir) )]      of string;
  TStrKeyWinApp   = array[0..Integer( High(TKeyWinApp) )]   of string;
  TStrKeyElite    = array[0..Integer( High(TKeyElite) )]    of string;
  TStrKeyTimer    = array[0..Integer( High(TKeyTimer) )]    of string;


var
  StrKeyIdent    : TStrKeyIdent;
  StrKeyEditor   : TStrKeyEditor;
  StrKeyFx       : TStrKeyFx;
  StrKeyAlpha    : TStrKeyAlpha;
  StrKeyDir      : TStrKeyDir;
  StrKeyWinApp   : TStrKeyWinApp;
  StrKeyElite    : TStrKeyElite;
  StrKeyTimer    : TStrKeyTimer;
  KeyCodes   : TKeyCodes = (
   (Car: #0;       LowerCode: '0';        UpperCode: '0'),
   {no upper distinction}
   (Car: '''';     LowerCode: '039';      UpperCode: '039'),
   (Car: '"';      LowerCode: '034';      UpperCode: '034'),
   (Car: '%';      LowerCode: '037';      UpperCode: '037'),
   (Car: '+';      LowerCode: '043';      UpperCode: '043'),
   (Car: '-';      LowerCode: '045';      UpperCode: '045'),
   (Car: '/';      LowerCode: '047';      UpperCode: '047'),
   (Car: '*';      LowerCode: '042';      UpperCode: '042'),
   (Car: '=';      LowerCode: '061';      UpperCode: '061'),
   (Car: '#';      LowerCode: '035';      UpperCode: '035'),
   (Car: '$';      LowerCode: '036';      UpperCode: '036'),
   (Car: '£';      LowerCode: '0163';     UpperCode: '0163'),
   (Car: '¥';      LowerCode: '0165';     UpperCode: '0165'),
   (Car: '!';      LowerCode: '033';      UpperCode: '033'),
   (Car: '?';      LowerCode: '063';      UpperCode: '063'),
   (Car: ',';      LowerCode: '044';      UpperCode: '044'),
   (Car: '.';      LowerCode: '046';      UpperCode: '046'),
   (Car: ':';      LowerCode: '058';      UpperCode: '058'),
   (Car: ';';      LowerCode: '059';      UpperCode: '059'),
   (Car: '@';      LowerCode: '064';      UpperCode: '064'),
   (Car: '^';      LowerCode: '094';      UpperCode: '094'),
   (Car: '\';      LowerCode: '092';      UpperCode: '092'),
   (Car: '°';      LowerCode: '0176';     UpperCode: '0176'),
   (Car: '§';      LowerCode: '0167';     UpperCode: '0167'),
   (Car: 'µ';      LowerCode: '0181';     UpperCode: '0181'),
   (Car: '_';      LowerCode: '095';      UpperCode: '095'),
   (Car: '(';      LowerCode: '040';      UpperCode: '040'),
   (Car: ')';      LowerCode: '041';      UpperCode: '041'),
   (Car: '[';      LowerCode: '091';      UpperCode: '091'),
   (Car: ']';      LowerCode: '093';      UpperCode: '093'),
   (Car: '{';      LowerCode: '0123';     UpperCode: '0123'),
   (Car: '}';      LowerCode: '0125';     UpperCode: '0125'),
   (Car: '&';      LowerCode: '038';      UpperCode: '038'),
   (Car: '<';      LowerCode: '060';      UpperCode: '060'),
   (Car: '>';      LowerCode: '062';      UpperCode: '062'),
   (Car: '|';      LowerCode: '0124';     UpperCode: '0124'),
   (Car: '³';      LowerCode: '0179';     UpperCode: '0179'),
   (Car: '¹';      LowerCode: '0185';     UpperCode: '0185'),
   (Car: '¨';      LowerCode: '0168';     UpperCode: '0168'),
   (Car: '«';      LowerCode: '0171';     UpperCode: '0171'),
   (Car: '»';      LowerCode: '0187';     UpperCode: '0187'),
   (Car: '÷';      LowerCode: '0247';     UpperCode: '0247'),
   (Car: '®';      LowerCode: '0174';     UpperCode: '0174'),
   (Car: '¶';      LowerCode: '0182';     UpperCode: '0182'),
   (Car: '×';      LowerCode: '0215';     UpperCode: '0215'),
   (Car: '©';      LowerCode: '0169';     UpperCode: '0169'),
   (Car: '±';      LowerCode: '0177';     UpperCode: '0177'),
   (Car: '¯';      LowerCode: '0175';     UpperCode: '0175'),
   (Car: '½';      LowerCode: '0189';     UpperCode: '0189'),
   (Car: '¼';      LowerCode: '0188';     UpperCode: '0188'),
   (Car: '¾';      LowerCode: '0190';     UpperCode: '0190'),
   (Car: '~';      LowerCode: '0126';     UpperCode: '0126'),
   (Car: 'ƒ';      LowerCode: '0131';     UpperCode: '0131'),


   {upper distinction}
   (Car: 'à';      LowerCode: '0224';     UpperCode: '0192'),
   (Car: 'â';      LowerCode: '0226';     UpperCode: '0194'),
   (Car: 'ä';      LowerCode: '0228';     UpperCode: '0196'),
   (Car: 'ç';      LowerCode: '0231';     UpperCode: '0199'),
   (Car: 'è';      LowerCode: '0232';     UpperCode: '0200'),
   (Car: 'é';      LowerCode: '0233';     UpperCode: '0201'),
   (Car: 'ê';      LowerCode: '0234';     UpperCode: '0202'),
   (Car: 'ë';      LowerCode: '0235';     UpperCode: '0203'),
   (Car: 'î';      LowerCode: '0238';     UpperCode: '0206'),
   (Car: 'ï';      LowerCode: '0239';     UpperCode: '0207'),
   (Car: 'ô';      LowerCode: '0244';     UpperCode: '0212'),
   (Car: 'ù';      LowerCode: '0249';     UpperCode: '0217'),
   (Car: 'û';      LowerCode: '0251';     UpperCode: '0219'),
   (Car: 'æ';      LowerCode: '0230';     UpperCode: '0198'),
   (Car: 'œ';      LowerCode: '0156';     UpperCode: '0140'),

   (Car: 'ø';      LowerCode: '0248';     UpperCode: '0216'),
   (Car: 'å';      LowerCode: '0229';     UpperCode: '0197'),
   (Car: 'ñ';      LowerCode: '0241';     UpperCode: '0209')
  );


type
  TKindOfFunctions = set of TKeyFx;

var
  Kof_Fx                : TKindOfFunctions = [kx_f1..kx_f24];
  Kof_Shift_Fx          : TKindOfFunctions = [kx_shift_f1..kx_shift_f24];
  Kof_Alt_Fx            : TKindOfFunctions = [kx_alt_f1..kx_alt_f24];
  Kof_Ctrl_Fx           : TKindOfFunctions = [kx_ctrl_f1..kx_ctrl_f24];
  Kof_Shift_Alt_Fx      : TKindOfFunctions = [kx_shalt_f1..kx_shalt_f24];
  Kof_Ctrl_Alt_Fx       : TKindOfFunctions = [kx_ctalt_f1..kx_ctalt_f24];
  Kof_Shift_Ctrl_Fx     : TKindOfFunctions = [kx_shctrl_f1..kx_shctrl_f24];
  Kof_Shift_alt_Ctrl_Fx : TKindOfFunctions = [kx_sac_f1..kx_sac_f24];

function ModuloOfFunc(const Value: TKeyFx):TKeyFx;

  function Translate(const Classe: TKeyFx):TKeyFx; begin
    Result := TKeyFx(Integer(kx_f1) + Integer(Value) - Integer(Classe))
  end;

begin
  Result := kx_none;
  if Value in Kof_Fx                 then Result := Value
    else
  if Value in Kof_Shift_Fx           then Result := Translate(kx_shift_f1)
    else
  if Value in Kof_Alt_Fx             then Result := Translate(kx_alt_f1)
    else
  if Value in Kof_Ctrl_Fx            then Result := Translate(kx_ctrl_f1)
    else
  if Value in Kof_Shift_Alt_Fx       then Result := Translate(kx_shalt_f1)
    else
  if Value in Kof_Ctrl_Alt_Fx        then Result := Translate(kx_ctalt_f1)
    else
  if Value in Kof_Shift_Ctrl_Fx      then Result := Translate(kx_shctrl_f1)
    else
  if Value in Kof_Shift_alt_Ctrl_Fx  then Result := Translate(kx_sac_f1)
end; {ModuloOfFunc}

function StrArrayToStr(const X: array of string): string;
var
  i : Integer;
begin
  with TStringList.Create do
  try
    for i := Low(X) + 1 to High(X) do Add( X[i] );
    Result := Trim( Text );
  finally
    Free
  end
end;

procedure AltRelease_;
begin
  keybd_event(VK_LMENU, MapVirtualKey(VK_LMENU, 0), KEYEVENTF_KEYUP, 0);
  KeyWrite(ParamKey, 'Alt_Tab', False)
end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

var
  ALLOAWED_ALT_TAB : array[0..16] of string =
    ( 'left',  'right',   'tab',    'shift_tab',  'activsoft',    'end',
      'home',  'tohome',  'toend',  'tophome',    'bottomend',    'up',
      'down',  'again',   'again2', 'again3',     'again5'
    );

function AllowedCommand(const Command: string; AllowedList: array of string):Boolean;
begin
  Result := IndexStr(AnsiLowerCase(Command), AllowedList) > -1
end;

var
  AGAIN_NOTALLAWED : array[0..3] of string =
    ( 'esc',   'return',  'close',  'alt_f4');

function AllowedAgain(const Command: string):Boolean;
begin
  Result := not AllowedCommand(Command, AGAIN_NOTALLAWED)
end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}


{ --- Current Charkey command --- }

procedure CurrentKeyCmdToReg(const Value: string);
begin
  KeyWrite(AppKey, 'LastKeyFunction', Value)
end;

function CurrentKeyFromReg:string;
begin
  Result := KeyReadString(AppKey, 'LastKeyFunction')
end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

procedure AltCheck(const AStr: string);
begin
  if not AllowedCommand(AStr, ALLOAWED_ALT_TAB) then AltRelease_
end;

procedure ExecProcessus(ProcName: string; Params: string);
{Les événements ne sont exécutés que si le fichier du preocessus existe}
begin
  try
    ShellExecute(0,'Open',PWideChar(ProcName),PWideChar(Params),nil,SW_SHOWDEFAULT);
  except
  end;
end;


procedure StrKeyTimerInitialize;
begin
//  StrKeyElite[ Integer(kt_none) ]             := 'timer';
end;

procedure StrKeyEliteInitialize;
begin
//  StrKeyElite[ Integer(ke_none) ]             := 'elite';

end;

procedure StrKeyWinAppInitialize;
begin
  StrKeyWinApp[ Integer(kx_win) ]            := 'win';
  StrKeyWinApp[ Integer(kx_wind) ]           := 'win_d';
  StrKeyWinApp[ Integer(kx_winm) ]           := 'win_m';
  StrKeyWinApp[ Integer(kx_wine) ]           := 'win_e';
  StrKeyWinApp[ Integer(kx_winl) ]           := 'win_l';
  StrKeyWinApp[ Integer(kx_winr) ]           := 'win_r';
  StrKeyWinApp[ Integer(kx_winp) ]           := 'win_p';
  StrKeyWinApp[ Integer(kx_winf) ]           := 'win_f';
  StrKeyWinApp[ Integer(kx_win1) ]           := 'win_1';
  StrKeyWinApp[ Integer(kx_win2) ]           := 'win_2';
  StrKeyWinApp[ Integer(kx_win3) ]           := 'win_3';
  StrKeyWinApp[ Integer(kx_win4) ]           := 'win_4';
  StrKeyWinApp[ Integer(kx_win5) ]           := 'win_5';
  StrKeyWinApp[ Integer(kx_win6) ]           := 'win_6';
  StrKeyWinApp[ Integer(kx_win7) ]           := 'win_7';
  StrKeyWinApp[ Integer(kx_win8) ]           := 'win_8';
  StrKeyWinApp[ Integer(kx_win9) ]           := 'win_9';
  StrKeyWinApp[ Integer(kx_win0) ]           := 'win_0';
  StrKeyWinApp[ Integer(kx_minimize) ]       := 'minimize';
  StrKeyWinApp[ Integer(kx_restore) ]        := 'restore';
  StrKeyWinApp[ Integer(kx_close) ]          := 'close';
  StrKeyWinApp[ Integer(kx_medplay) ]        := 'mediaplay';
  StrKeyWinApp[ Integer(kx_medstop) ]        := 'mediastop';
  StrKeyWinApp[ Integer(kx_mednext) ]        := 'medianext';
  StrKeyWinApp[ Integer(kx_medprior) ]       := 'mediaprior';
  StrKeyWinApp[ Integer(kx_volmute) ]        := 'volume_mute';
  StrKeyWinApp[ Integer(kx_voldown) ]        := 'volume_down';
  StrKeyWinApp[ Integer(kx_volup) ]          := 'volume_up';
  StrKeyWinApp[ Integer(kx_navback) ]        := 'navigator_back';
  StrKeyWinApp[ Integer(kx_navforward) ]     := 'navigator_forward';
  StrKeyWinApp[ Integer(kx_navrefresh) ]     := 'navigator_refresh';
  StrKeyWinApp[ Integer(kx_navstop) ]        := 'navigator_stop';
  StrKeyWinApp[ Integer(kx_navsearch) ]      := 'navigator_search';
  StrKeyWinApp[ Integer(kx_navfavor) ]       := 'navigator_favorite';
  StrKeyWinApp[ Integer(kx_navhome) ]        := 'navigator_home';
  StrKeyWinApp[ Integer(kx_navgolink) ]      := 'navigator_golink';
  StrKeyWinApp[ Integer(kx_navlnext) ]       := 'navigator_linknext';
  StrKeyWinApp[ Integer(kx_navlprior) ]      := 'navigator_linkprior';
  StrKeyWinApp[ Integer(kx_navfull) ]        := 'navigator_fullscreen';
  StrKeyWinApp[ Integer(kx_sheetclose) ]     := 'navigator_sheetclose';
  StrKeyWinApp[ Integer(kx_sheetnew) ]       := 'navigator_sheetnew';
  StrKeyWinApp[ Integer(kx_sheetnext) ]      := 'navigator_sheetnext';
  StrKeyWinApp[ Integer(kx_sheetprior) ]     := 'navigator_sheetprior';
  StrKeyWinApp[ Integer(kx_navnew) ]         := 'navigator_new';
  StrKeyWinApp[ Integer(kx_navzoomin) ]      := 'navigator_zoomin';
  StrKeyWinApp[ Integer(kx_navzoomout) ]     := 'navigator_zoomout';
  StrKeyWinApp[ Integer(kx_navnozoom) ]      := 'navigator_nozoom';
  StrKeyWinApp[ Integer(kx_left_clic) ]      := 'mouse_left';
  StrKeyWinApp[ Integer(kx_right_clic) ]     := 'mouse_right';
  StrKeyWinApp[ Integer(kx_middle_clic) ]    := 'mouse_middle';
  StrKeyWinApp[ Integer(kx_double_clic) ]    := 'mouse_double';
  StrKeyWinApp[ Integer(kx_notepad) ]        := 'notepad';

end;

procedure StrKeyIdentInitialize;
begin
  {no upper distinction}
  StrKeyIdent[ Integer(ki_quot) ]            := 'quot';
  StrKeyIdent[ Integer(ki_dquot) ]           := 'dquot';
  StrKeyIdent[ Integer(ki_pcent) ]           := '%';
  StrKeyIdent[ Integer(ki_plus) ]            := '+';
  StrKeyIdent[ Integer(ki_moins) ]           := '-';
  StrKeyIdent[ Integer(ki_slash) ]           := '/';
  StrKeyIdent[ Integer(ki_aster) ]           := '*';
  StrKeyIdent[ Integer(ki_equal) ]           := '=';
  StrKeyIdent[ Integer(ki_diese) ]           := '#';
  StrKeyIdent[ Integer(ki_dolar) ]           := '$';
  StrKeyIdent[ Integer(ki_livre) ]           := '£';
  StrKeyIdent[ Integer(ki_yen) ]             := '¥';
  StrKeyIdent[ Integer(ki_excla) ]           := '!';
  StrKeyIdent[ Integer(ki_intero) ]          := '?';
  StrKeyIdent[ Integer(ki_virgul) ]          := ',';
  StrKeyIdent[ Integer(ki_point) ]           := '.';
  StrKeyIdent[ Integer(ki_point2) ]          := ':';
  StrKeyIdent[ Integer(ki_semic) ]           := ';';
  StrKeyIdent[ Integer(ki_arobas) ]          := '@';
  StrKeyIdent[ Integer(ki_chapo) ]           := '^';
  StrKeyIdent[ Integer(ki_antislash) ]       := '\';
  StrKeyIdent[ Integer(ki_degre) ]           := '°';
  StrKeyIdent[ Integer(ki_paragrf) ]         := '§';
  StrKeyIdent[ Integer(ki_mu) ]              := 'µ';
  StrKeyIdent[ Integer(ki_underscore) ]      := '_';
  StrKeyIdent[ Integer(ki_pargauch) ]        := '(';
  StrKeyIdent[ Integer(ki_pardroit) ]        := ')';
  StrKeyIdent[ Integer(ki_bragauch) ]        := '[';
  StrKeyIdent[ Integer(ki_bradroit) ]        := ']';
  StrKeyIdent[ Integer(ki_accgauch) ]        := '{';
  StrKeyIdent[ Integer(ki_accdroit) ]        := '}';
  StrKeyIdent[ Integer(ki_etcomerc) ]        := 'etcom';
  StrKeyIdent[ Integer(ki_infa) ]            := 'infa';
  StrKeyIdent[ Integer(ki_supa) ]            := 'supa';
  StrKeyIdent[ Integer(ki_pipe) ]            := 'pipe';
  StrKeyIdent[ Integer(ki_cube) ]            := '³';
  StrKeyIdent[ Integer(ki_puis1) ]           := '¹';
  StrKeyIdent[ Integer(ki_trema) ]           := '¨';
  StrKeyIdent[ Integer(ki_guigauch) ]        := '«';
  StrKeyIdent[ Integer(ki_guidroit) ]        := '»';
  StrKeyIdent[ Integer(ki_divise) ]          := '÷';
  StrKeyIdent[ Integer(ki_trademark) ]       := '®';
  StrKeyIdent[ Integer(ki_pi) ]              := '¶';
  StrKeyIdent[ Integer(ki_multipli) ]        := '×';
  StrKeyIdent[ Integer(ki_copyright) ]       := '©';
  StrKeyIdent[ Integer(ki_plusmoins) ]       := '±';
  StrKeyIdent[ Integer(ki_macron) ]          := '¯';
  StrKeyIdent[ Integer(ki_1demi) ]           := '½';
  StrKeyIdent[ Integer(ki_1quart) ]          := '¼';
  StrKeyIdent[ Integer(ki_3quart) ]          := '¾';
  StrKeyIdent[ Integer(ki_tilde) ]           := '~';
  StrKeyIdent[ Integer(ki_function) ]        := 'ƒ';


  {upper distinction}
  StrKeyIdent[ Integer(ki_agrav) ]           := 'à';
  StrKeyIdent[ Integer(ki_acircon) ]         := 'â';
  StrKeyIdent[ Integer(ki_atrema) ]          := 'ä';
  StrKeyIdent[ Integer(ki_ccedil) ]          := 'ç';
  StrKeyIdent[ Integer(ki_egrav) ]           := 'è';
  StrKeyIdent[ Integer(ki_eaigu) ]           := 'é';
  StrKeyIdent[ Integer(ki_ecircon) ]         := 'ê';
  StrKeyIdent[ Integer(ki_etrema) ]          := 'ë';
  StrKeyIdent[ Integer(ki_icircon) ]         := 'î';
  StrKeyIdent[ Integer(ki_itrema) ]          := 'ï';
  StrKeyIdent[ Integer(ki_ocircon) ]         := 'ô';
  StrKeyIdent[ Integer(ki_ugrav) ]           := 'ù';
  StrKeyIdent[ Integer(ki_ucircon) ]         := 'û';
  StrKeyIdent[ Integer(ki_aelie) ]           := 'æ';
  StrKeyIdent[ Integer(ki_oelie) ]           := 'œ';
  StrKeyIdent[ Integer(ki_letempty) ]        := 'ø';
  StrKeyIdent[ Integer(ki_angstroem) ]       := 'å';
  StrKeyIdent[ Integer(ki_nenj) ]            := 'ñ';
end;

procedure StrKeyEditorInitialize;
begin
  StrKeyEditor[ Integer(kf_none) ]           := 'none';
  StrKeyEditor[ Integer(kf_selectall) ]      := 'selectall';
  StrKeyEditor[ Integer(kf_copy) ]           := 'copy';
  StrKeyEditor[ Integer(kf_paste) ]          := 'paste';
  StrKeyEditor[ Integer(kf_cut) ]            := 'cut';
  StrKeyEditor[ Integer(kf_unselect) ]       := 'unselect';
  StrKeyEditor[ Integer(kf_clear) ]          := 'clear';
  StrKeyEditor[ Integer(kf_home) ]           := 'tohome';
  StrKeyEditor[ Integer(kf_end) ]            := 'toend';
  StrKeyEditor[ Integer(kf_tophome) ]        := 'tophome';
  StrKeyEditor[ Integer(kf_bottomend) ]      := 'bottomend';
  StrKeyEditor[ Integer(kf_seltoend) ]       := 'seltoend';
  StrKeyEditor[ Integer(kf_selalltoend) ]    := 'selalltoend';
  StrKeyEditor[ Integer(kf_seltohome) ]      := 'seltohome';
  StrKeyEditor[ Integer(kf_selalltohome) ]   := 'selalltohome';
  StrKeyEditor[ Integer(kf_selhomepage) ]    := 'selhomepage';
  StrKeyEditor[ Integer(kf_selendpage) ]     := 'selendpage';
  StrKeyEditor[ Integer(kf_penon) ]          := 'penon';
  StrKeyEditor[ Integer(kf_penoff) ]         := 'penoff';
  StrKeyEditor[ Integer(kf_selword) ]        := 'selword';
  StrKeyEditor[ Integer(kf_wordnext) ]       := 'wordnext';
  StrKeyEditor[ Integer(kf_wordprior) ]      := 'wordprior';
  StrKeyEditor[ Integer(kf_alt) ]            := 'alt';
  StrKeyEditor[ Integer(kf_search) ]         := 'search';
  StrKeyEditor[ Integer(kf_selsearch) ]      := 'selsearch';
  StrKeyEditor[ Integer(kf_searchnext) ]     := 'searchnext';
  StrKeyEditor[ Integer(kf_menuopen) ]       := 'menuopen';
  StrKeyEditor[ Integer(kf_menuclose) ]      := 'menuclose';
  StrKeyEditor[ Integer(kf_ctrl) ]           := 'ctrl';
  StrKeyEditor[ Integer(kf_altrelease) ]     := 'altrelease';
  StrKeyEditor[ Integer(kf_activsoft) ]      := 'activsoft';
  StrKeyEditor[ Integer(kf_again) ]          := 'again';
  StrKeyEditor[ Integer(kf_again2) ]         := 'again2';
  StrKeyEditor[ Integer(kf_again3) ]         := 'again3';
  StrKeyEditor[ Integer(kf_again5) ]         := 'again5';
end;

procedure StrKeyDirInitialize;
begin
  StrKeyDir[ Integer(kd_left) ]                 := 'left';
  StrKeyDir[ Integer(kd_right) ]                := 'right';
  StrKeyDir[ Integer(kd_up) ]                   := 'up';
  StrKeyDir[ Integer(kd_down) ]                 := 'down';
  StrKeyDir[ Integer(kd_home) ]                 := 'home';
  StrKeyDir[ Integer(kd_end) ]                  := 'end';
  StrKeyDir[ Integer(kd_esc) ]                  := 'esc';
  StrKeyDir[ Integer(kd_return) ]               := 'return';
  StrKeyDir[ Integer(kd_tab) ]                  := 'tab';
  StrKeyDir[ Integer(kd_pagenext) ]             := 'pagenext';
  StrKeyDir[ Integer(kd_pageprior) ]            := 'pageprior';
  StrKeyDir[ Integer(kd_space) ]                := 'space';
  StrKeyDir[ Integer(kd_backspace) ]            := 'backspace';
  StrKeyDir[ Integer(kd_suppr) ]                := 'suppr';
  StrKeyDir[ Integer(kd_insert) ]               := 'inser';
  StrKeyDir[ Integer(kd_capital) ]              := 'capital';
  StrKeyDir[ Integer(kd_numlock) ]              := 'numlock';
  {Shiht+Code}
  StrKeyDir[ Integer(kd_shift_left) ]           := 'shift_left';
  StrKeyDir[ Integer(kd_shift_right) ]          := 'shift_right';
  StrKeyDir[ Integer(kd_shift_up) ]             := 'shift_up';
  StrKeyDir[ Integer(kd_shift_down) ]           := 'shift_down';
  StrKeyDir[ Integer(kd_shift_home) ]           := 'shift_home';
  StrKeyDir[ Integer(kd_shift_end) ]            := 'shift_end';
  StrKeyDir[ Integer(kd_shift_esc) ]            := 'shift_esc';
  StrKeyDir[ Integer(kd_shift_return) ]         := 'shift_return';
  StrKeyDir[ Integer(kd_shift_tab) ]            := 'shift_tab';
  StrKeyDir[ Integer(kd_shift_pagenext) ]       := 'shift_pagenext';
  StrKeyDir[ Integer(kd_shift_pageprior) ]      := 'shift_pageprior';
  StrKeyDir[ Integer(kd_shift_space) ]          := 'shift_space';
  StrKeyDir[ Integer(kd_shift_backspace) ]      := 'shift_backspace';
  StrKeyDir[ Integer(kd_shift_suppr) ]          := 'shift_suppr';
  StrKeyDir[ Integer(kd_shift_insert) ]         := 'shift_inser';
  StrKeyDir[ Integer(kd_shift_capital) ]        := 'shift_capital';
  StrKeyDir[ Integer(kd_shift_numlock) ]        := 'shift_numlock';
  {Ctrl+Code}
  StrKeyDir[ Integer(kd_ctrl_left) ]            := 'ctrl_left';
  StrKeyDir[ Integer(kd_ctrl_right) ]           := 'ctrl_right';
  StrKeyDir[ Integer(kd_ctrl_up) ]              := 'ctrl_up';
  StrKeyDir[ Integer(kd_ctrl_down) ]            := 'ctrl_down';
  StrKeyDir[ Integer(kd_ctrl_home) ]            := 'ctrl_home';
  StrKeyDir[ Integer(kd_ctrl_end) ]             := 'ctrl_end';
  StrKeyDir[ Integer(kd_ctrl_esc) ]             := 'ctrl_esc';
  StrKeyDir[ Integer(kd_ctrl_return) ]          := 'ctrl_return';
  StrKeyDir[ Integer(kd_ctrl_tab) ]             := 'ctrl_tab';
  StrKeyDir[ Integer(kd_ctrl_pagenext) ]        := 'ctrl_pagenext';
  StrKeyDir[ Integer(kd_ctrl_pageprior) ]       := 'ctrl_pageprior';
  StrKeyDir[ Integer(kd_ctrl_space) ]           := 'ctrl_space';
  StrKeyDir[ Integer(kd_ctrl_backspace) ]       := 'ctrl_backspace';
  StrKeyDir[ Integer(kd_ctrl_suppr) ]           := 'ctrl_suppr';
  StrKeyDir[ Integer(kd_ctrl_insert) ]          := 'ctrl_inser';
  StrKeyDir[ Integer(kd_ctrl_capital) ]         := 'ctrl_capital';
  StrKeyDir[ Integer(kd_ctrl_numlock) ]         := 'ctrl_numlock';
  {Alt+Code}
  StrKeyDir[ Integer(kd_alt_left) ]             := 'alt_left';
  StrKeyDir[ Integer(kd_alt_right) ]            := 'alt_right';
  StrKeyDir[ Integer(kd_alt_up) ]               := 'alt_up';
  StrKeyDir[ Integer(kd_alt_down) ]             := 'alt_down';
  StrKeyDir[ Integer(kd_alt_home) ]             := 'alt_home';
  StrKeyDir[ Integer(kd_alt_end) ]              := 'alt_end';
  StrKeyDir[ Integer(kd_alt_esc) ]              := 'alt_esc';
  StrKeyDir[ Integer(kd_alt_return) ]           := 'alt_return';
  StrKeyDir[ Integer(kd_alt_tab) ]              := 'alt_tab';
  StrKeyDir[ Integer(kd_alt_pagenext) ]         := 'alt_pagenext';
  StrKeyDir[ Integer(kd_alt_pageprior) ]        := 'alt_pageprior';
  StrKeyDir[ Integer(kd_alt_space) ]            := 'alt_space';
  StrKeyDir[ Integer(kd_alt_backspace) ]        := 'alt_backspace';
  StrKeyDir[ Integer(kd_alt_suppr) ]            := 'alt_suppr';
  StrKeyDir[ Integer(kd_alt_insert) ]           := 'alt_inser';
  StrKeyDir[ Integer(kd_alt_capital) ]          := 'alt_capital';
  StrKeyDir[ Integer(kd_alt_numlock) ]          := 'alt_numlock';
  {Shift+Ctrl+Code}
  StrKeyDir[ Integer(kd_shctrl_left) ]          := 'shift_ctrl_left';
  StrKeyDir[ Integer(kd_shctrl_right) ]         := 'shift_ctrl_right';
  StrKeyDir[ Integer(kd_shctrl_up) ]            := 'shift_ctrl_up';
  StrKeyDir[ Integer(kd_shctrl_down) ]          := 'shift_ctrl_down';
  StrKeyDir[ Integer(kd_shctrl_home) ]          := 'shift_ctrl_home';
  StrKeyDir[ Integer(kd_shctrl_end) ]           := 'shift_ctrl_end';
  StrKeyDir[ Integer(kd_shctrl_esc) ]           := 'shift_ctrl_esc';
  StrKeyDir[ Integer(kd_shctrl_return) ]        := 'shift_ctrl_return';
  StrKeyDir[ Integer(kd_shctrl_tab) ]           := 'shift_ctrl_tab';
  StrKeyDir[ Integer(kd_shctrl_pagenext) ]      := 'shift_ctrl_pagenext';
  StrKeyDir[ Integer(kd_shctrl_pageprior) ]     := 'shift_ctrl_pageprior';
  StrKeyDir[ Integer(kd_shctrl_space) ]         := 'shift_ctrl_space';
  StrKeyDir[ Integer(kd_shctrl_backspace) ]     := 'shift_ctrl_backspace';
  StrKeyDir[ Integer(kd_shctrl_suppr) ]         := 'shift_ctrl_suppr';
  StrKeyDir[ Integer(kd_shctrl_insert) ]        := 'shift_ctrl_inser';
  StrKeyDir[ Integer(kd_shctrl_capital) ]       := 'shift_ctrl_capital';
  StrKeyDir[ Integer(kd_shctrl_numlock) ]       := 'shift_ctrl_numlock';
  {Shift+Alt+Code}
  StrKeyDir[ Integer(kd_shalt_left) ]           := 'shift_alt_left';
  StrKeyDir[ Integer(kd_shalt_right) ]          := 'shift_alt_right';
  StrKeyDir[ Integer(kd_shalt_up) ]             := 'shift_alt_up';
  StrKeyDir[ Integer(kd_shalt_down) ]           := 'shift_alt_down';
  StrKeyDir[ Integer(kd_shalt_home) ]           := 'shift_alt_home';
  StrKeyDir[ Integer(kd_shalt_end) ]            := 'shift_alt_end';
  StrKeyDir[ Integer(kd_shalt_esc) ]            := 'shift_alt_esc';
  StrKeyDir[ Integer(kd_shalt_return) ]         := 'shift_alt_return';
  StrKeyDir[ Integer(kd_shalt_tab) ]            := 'shift_alt_tab';
  StrKeyDir[ Integer(kd_shalt_pagenext) ]       := 'shift_alt_pagenext';
  StrKeyDir[ Integer(kd_shalt_pageprior) ]      := 'shift_alt_pageprior';
  StrKeyDir[ Integer(kd_shalt_space) ]          := 'shift_alt_space';
  StrKeyDir[ Integer(kd_shalt_backspace) ]      := 'shift_alt_backspace';
  StrKeyDir[ Integer(kd_shalt_suppr) ]          := 'shift_alt_suppr';
  StrKeyDir[ Integer(kd_shalt_insert) ]         := 'shift_alt_inser';
  StrKeyDir[ Integer(kd_shalt_capital) ]        := 'shift_alt_capital';
  StrKeyDir[ Integer(kd_shalt_numlock) ]        := 'shift_alt_numlock';
  {Alt+Ctrl+Code}
  StrKeyDir[ Integer(kd_alctrl_left) ]          := 'alt_ctrl_left';
  StrKeyDir[ Integer(kd_alctrl_right) ]         := 'alt_ctrl_right';
  StrKeyDir[ Integer(kd_alctrl_up) ]            := 'alt_ctrl_up';
  StrKeyDir[ Integer(kd_alctrl_down) ]          := 'alt_ctrl_down';
  StrKeyDir[ Integer(kd_alctrl_home) ]          := 'alt_ctrl_home';
  StrKeyDir[ Integer(kd_alctrl_end) ]           := 'alt_ctrl_end';
  StrKeyDir[ Integer(kd_alctrl_esc) ]           := 'alt_ctrl_esc';
  StrKeyDir[ Integer(kd_alctrl_return) ]        := 'alt_ctrl_return';
  StrKeyDir[ Integer(kd_alctrl_tab) ]           := 'alt_ctrl_tab';
  StrKeyDir[ Integer(kd_alctrl_pagenext) ]      := 'alt_ctrl_pagenext';
  StrKeyDir[ Integer(kd_alctrl_pageprior) ]     := 'alt_ctrl_pageprior';
  StrKeyDir[ Integer(kd_alctrl_space) ]         := 'alt_ctrl_space';
  StrKeyDir[ Integer(kd_alctrl_backspace) ]     := 'alt_ctrl_backspace';
  StrKeyDir[ Integer(kd_alctrl_suppr) ]         := 'alt_ctrl_suppr';
  StrKeyDir[ Integer(kd_alctrl_insert) ]        := 'alt_ctrl_inser';
  StrKeyDir[ Integer(kd_alctrl_capital) ]       := 'alt_ctrl_capital';
  StrKeyDir[ Integer(kd_alctrl_numlock) ]       := 'alt_ctrl_numlock';
  {Shift+Alt+Ctrl+Code -- non implémentés vocalement }
  StrKeyDir[ Integer(kd_sac_left) ]             := 'shift_alt_ctrl_left';
  StrKeyDir[ Integer(kd_sac_right) ]            := 'shift_alt_ctrl_right';
  StrKeyDir[ Integer(kd_sac_up) ]               := 'shift_alt_ctrl_up';
  StrKeyDir[ Integer(kd_sac_down) ]             := 'shift_alt_ctrl_down';
  StrKeyDir[ Integer(kd_sac_home) ]             := 'shift_alt_ctrl_home';
  StrKeyDir[ Integer(kd_sac_end) ]              := 'shift_alt_ctrl_end';
  StrKeyDir[ Integer(kd_sac_esc) ]              := 'shift_alt_ctrl_esc';
  StrKeyDir[ Integer(kd_sac_return) ]           := 'shift_alt_ctrl_return';
  StrKeyDir[ Integer(kd_sac_tab) ]              := 'shift_alt_ctrl_tab';
  StrKeyDir[ Integer(kd_sac_pagenext) ]         := 'shift_alt_ctrl_pagenext';
  StrKeyDir[ Integer(kd_sac_pageprior) ]        := 'shift_alt_ctrl_pageprior';
  StrKeyDir[ Integer(kd_sac_space) ]            := 'shift_alt_ctrl_space';
  StrKeyDir[ Integer(kd_sac_backspace) ]        := 'shift_alt_ctrl_backspace';
  StrKeyDir[ Integer(kd_sac_suppr) ]            := 'shift_alt_ctrl_suppr';
  StrKeyDir[ Integer(kd_sac_insert) ]           := 'shift_alt_ctrl_inser';
  StrKeyDir[ Integer(kd_sac_capital) ]          := 'shift_alt_ctrl_capital';
  StrKeyDir[ Integer(kd_sac_numlock ) ]          := 'shift_alt_ctrl_numlock';
end;

procedure StrKeyAlphaInitialize;
begin
  {Ctrl+alpha}
  StrKeyAlpha[ Integer(ka_ctrl_a) ]       := 'ctrl_a';
  StrKeyAlpha[ Integer(ka_ctrl_b) ]       := 'ctrl_b';
  StrKeyAlpha[ Integer(ka_ctrl_c) ]       := 'ctrl_c';
  StrKeyAlpha[ Integer(ka_ctrl_d) ]       := 'ctrl_d';
  StrKeyAlpha[ Integer(ka_ctrl_e) ]       := 'ctrl_e';
  StrKeyAlpha[ Integer(ka_ctrl_f) ]       := 'ctrl_f';
  StrKeyAlpha[ Integer(ka_ctrl_g) ]       := 'ctrl_g';
  StrKeyAlpha[ Integer(ka_ctrl_h) ]       := 'ctrl_h';
  StrKeyAlpha[ Integer(ka_ctrl_i) ]       := 'ctrl_i';
  StrKeyAlpha[ Integer(ka_ctrl_j) ]       := 'ctrl_j';
  StrKeyAlpha[ Integer(ka_ctrl_k) ]       := 'ctrl_k';
  StrKeyAlpha[ Integer(ka_ctrl_l) ]       := 'ctrl_l';
  StrKeyAlpha[ Integer(ka_ctrl_m) ]       := 'ctrl_m';
  StrKeyAlpha[ Integer(ka_ctrl_n) ]       := 'ctrl_n';
  StrKeyAlpha[ Integer(ka_ctrl_o) ]       := 'ctrl_o';
  StrKeyAlpha[ Integer(ka_ctrl_p) ]       := 'ctrl_p';
  StrKeyAlpha[ Integer(ka_ctrl_q) ]       := 'ctrl_q';
  StrKeyAlpha[ Integer(ka_ctrl_r) ]       := 'ctrl_r';
  StrKeyAlpha[ Integer(ka_ctrl_s) ]       := 'ctrl_s';
  StrKeyAlpha[ Integer(ka_ctrl_t) ]       := 'ctrl_t';
  StrKeyAlpha[ Integer(ka_ctrl_u) ]       := 'ctrl_u';
  StrKeyAlpha[ Integer(ka_ctrl_v) ]       := 'ctrl_v';
  StrKeyAlpha[ Integer(ka_ctrl_w) ]       := 'ctrl_w';
  StrKeyAlpha[ Integer(ka_ctrl_x) ]       := 'ctrl_x';
  StrKeyAlpha[ Integer(ka_ctrl_y) ]       := 'ctrl_y';
  StrKeyAlpha[ Integer(ka_ctrl_z) ]       := 'ctrl_z';
  {Shift+alpha}
  StrKeyAlpha[ Integer(ka_shift_a) ]      := 'shift_a';
  StrKeyAlpha[ Integer(ka_shift_b) ]      := 'shift_b';
  StrKeyAlpha[ Integer(ka_shift_c) ]      := 'shift_c';
  StrKeyAlpha[ Integer(ka_shift_d) ]      := 'shift_d';
  StrKeyAlpha[ Integer(ka_shift_e) ]      := 'shift_e';
  StrKeyAlpha[ Integer(ka_shift_f) ]      := 'shift_f';
  StrKeyAlpha[ Integer(ka_shift_g) ]      := 'shift_g';
  StrKeyAlpha[ Integer(ka_shift_h) ]      := 'shift_h';
  StrKeyAlpha[ Integer(ka_shift_i) ]      := 'shift_i';
  StrKeyAlpha[ Integer(ka_shift_j) ]      := 'shift_j';
  StrKeyAlpha[ Integer(ka_shift_k) ]      := 'shift_k';
  StrKeyAlpha[ Integer(ka_shift_l) ]      := 'shift_l';
  StrKeyAlpha[ Integer(ka_shift_m) ]      := 'shift_m';
  StrKeyAlpha[ Integer(ka_shift_n) ]      := 'shift_n';
  StrKeyAlpha[ Integer(ka_shift_o) ]      := 'shift_o';
  StrKeyAlpha[ Integer(ka_shift_p) ]      := 'shift_p';
  StrKeyAlpha[ Integer(ka_shift_q) ]      := 'shift_q';
  StrKeyAlpha[ Integer(ka_shift_r) ]      := 'shift_r';
  StrKeyAlpha[ Integer(ka_shift_s) ]      := 'shift_s';
  StrKeyAlpha[ Integer(ka_shift_t) ]      := 'shift_t';
  StrKeyAlpha[ Integer(ka_shift_u) ]      := 'shift_u';
  StrKeyAlpha[ Integer(ka_shift_v) ]      := 'shift_v';
  StrKeyAlpha[ Integer(ka_shift_w) ]      := 'shift_w';
  StrKeyAlpha[ Integer(ka_shift_x) ]      := 'shift_x';
  StrKeyAlpha[ Integer(ka_shift_y) ]      := 'shift_y';
  StrKeyAlpha[ Integer(ka_shift_z) ]      := 'shift_z';
  {Alt+alpha}    
  StrKeyAlpha[ Integer(ka_alt_a) ]        := 'alt_a';
  StrKeyAlpha[ Integer(ka_alt_b) ]        := 'alt_b';
  StrKeyAlpha[ Integer(ka_alt_c) ]        := 'alt_c';
  StrKeyAlpha[ Integer(ka_alt_d) ]        := 'alt_d';
  StrKeyAlpha[ Integer(ka_alt_e) ]        := 'alt_e';
  StrKeyAlpha[ Integer(ka_alt_f) ]        := 'alt_f';
  StrKeyAlpha[ Integer(ka_alt_g) ]        := 'alt_g';
  StrKeyAlpha[ Integer(ka_alt_h) ]        := 'alt_h';
  StrKeyAlpha[ Integer(ka_alt_i) ]        := 'alt_i';
  StrKeyAlpha[ Integer(ka_alt_j) ]        := 'alt_j';
  StrKeyAlpha[ Integer(ka_alt_k) ]        := 'alt_k';
  StrKeyAlpha[ Integer(ka_alt_l) ]        := 'alt_l';
  StrKeyAlpha[ Integer(ka_alt_m) ]        := 'alt_m';
  StrKeyAlpha[ Integer(ka_alt_n) ]        := 'alt_n';
  StrKeyAlpha[ Integer(ka_alt_o) ]        := 'alt_o';
  StrKeyAlpha[ Integer(ka_alt_p) ]        := 'alt_p';
  StrKeyAlpha[ Integer(ka_alt_q) ]        := 'alt_q';
  StrKeyAlpha[ Integer(ka_alt_r) ]        := 'alt_r';
  StrKeyAlpha[ Integer(ka_alt_s) ]        := 'alt_s';
  StrKeyAlpha[ Integer(ka_alt_t) ]        := 'alt_t';
  StrKeyAlpha[ Integer(ka_alt_u) ]        := 'alt_u';
  StrKeyAlpha[ Integer(ka_alt_v) ]        := 'alt_v';
  StrKeyAlpha[ Integer(ka_alt_w) ]        := 'alt_w';
  StrKeyAlpha[ Integer(ka_alt_x) ]        := 'alt_x';
  StrKeyAlpha[ Integer(ka_alt_y) ]        := 'alt_y';
  StrKeyAlpha[ Integer(ka_alt_z) ]        := 'alt_z';
  {Alt+Ctrl+Alpha}
  StrKeyAlpha[ Integer(ka_alt_ctrl_a) ]   := 'alt_ctrl_a';
  StrKeyAlpha[ Integer(ka_alt_ctrl_b) ]   := 'alt_ctrl_b';
  StrKeyAlpha[ Integer(ka_alt_ctrl_c) ]   := 'alt_ctrl_c';
  StrKeyAlpha[ Integer(ka_alt_ctrl_d) ]   := 'alt_ctrl_d';
  StrKeyAlpha[ Integer(ka_alt_ctrl_e) ]   := 'alt_ctrl_e';
  StrKeyAlpha[ Integer(ka_alt_ctrl_f) ]   := 'alt_ctrl_f';
  StrKeyAlpha[ Integer(ka_alt_ctrl_g) ]   := 'alt_ctrl_g';
  StrKeyAlpha[ Integer(ka_alt_ctrl_h) ]   := 'alt_ctrl_h';
  StrKeyAlpha[ Integer(ka_alt_ctrl_i) ]   := 'alt_ctrl_i';
  StrKeyAlpha[ Integer(ka_alt_ctrl_j) ]   := 'alt_ctrl_j';
  StrKeyAlpha[ Integer(ka_alt_ctrl_k) ]   := 'alt_ctrl_k';
  StrKeyAlpha[ Integer(ka_alt_ctrl_l) ]   := 'alt_ctrl_l';
  StrKeyAlpha[ Integer(ka_alt_ctrl_m) ]   := 'alt_ctrl_m';
  StrKeyAlpha[ Integer(ka_alt_ctrl_n) ]   := 'alt_ctrl_n';
  StrKeyAlpha[ Integer(ka_alt_ctrl_o) ]   := 'alt_ctrl_o';
  StrKeyAlpha[ Integer(ka_alt_ctrl_p) ]   := 'alt_ctrl_p';
  StrKeyAlpha[ Integer(ka_alt_ctrl_q) ]   := 'alt_ctrl_q';
  StrKeyAlpha[ Integer(ka_alt_ctrl_r) ]   := 'alt_ctrl_r';
  StrKeyAlpha[ Integer(ka_alt_ctrl_s) ]   := 'alt_ctrl_s';
  StrKeyAlpha[ Integer(ka_alt_ctrl_t) ]   := 'alt_ctrl_t';
  StrKeyAlpha[ Integer(ka_alt_ctrl_u) ]   := 'alt_ctrl_u';
  StrKeyAlpha[ Integer(ka_alt_ctrl_v) ]   := 'alt_ctrl_v';
  StrKeyAlpha[ Integer(ka_alt_ctrl_w) ]   := 'alt_ctrl_w';
  StrKeyAlpha[ Integer(ka_alt_ctrl_x) ]   := 'alt_ctrl_x';
  StrKeyAlpha[ Integer(ka_alt_ctrl_y) ]   := 'alt_ctrl_y';
  StrKeyAlpha[ Integer(ka_alt_ctrl_z) ]   := 'alt_ctrl_z';
  {Shift+Ctrl+Alpha}
  StrKeyAlpha[ Integer(ka_shift_ctrl_a) ] := 'shift_ctrl_a';
  StrKeyAlpha[ Integer(ka_shift_ctrl_b) ] := 'shift_ctrl_b';
  StrKeyAlpha[ Integer(ka_shift_ctrl_c) ] := 'shift_ctrl_c';
  StrKeyAlpha[ Integer(ka_shift_ctrl_d) ] := 'shift_ctrl_d';
  StrKeyAlpha[ Integer(ka_shift_ctrl_e) ] := 'shift_ctrl_e';
  StrKeyAlpha[ Integer(ka_shift_ctrl_f) ] := 'shift_ctrl_f';
  StrKeyAlpha[ Integer(ka_shift_ctrl_g) ] := 'shift_ctrl_g';
  StrKeyAlpha[ Integer(ka_shift_ctrl_h) ] := 'shift_ctrl_h';
  StrKeyAlpha[ Integer(ka_shift_ctrl_i) ] := 'shift_ctrl_i';
  StrKeyAlpha[ Integer(ka_shift_ctrl_j) ] := 'shift_ctrl_j';
  StrKeyAlpha[ Integer(ka_shift_ctrl_k) ] := 'shift_ctrl_k';
  StrKeyAlpha[ Integer(ka_shift_ctrl_l) ] := 'shift_ctrl_l';
  StrKeyAlpha[ Integer(ka_shift_ctrl_m) ] := 'shift_ctrl_m';
  StrKeyAlpha[ Integer(ka_shift_ctrl_n) ] := 'shift_ctrl_n';
  StrKeyAlpha[ Integer(ka_shift_ctrl_o) ] := 'shift_ctrl_o';
  StrKeyAlpha[ Integer(ka_shift_ctrl_p) ] := 'shift_ctrl_p';
  StrKeyAlpha[ Integer(ka_shift_ctrl_q) ] := 'shift_ctrl_q';
  StrKeyAlpha[ Integer(ka_shift_ctrl_r) ] := 'shift_ctrl_r';
  StrKeyAlpha[ Integer(ka_shift_ctrl_s) ] := 'shift_ctrl_s';
  StrKeyAlpha[ Integer(ka_shift_ctrl_t) ] := 'shift_ctrl_t';
  StrKeyAlpha[ Integer(ka_shift_ctrl_u) ] := 'shift_ctrl_u';
  StrKeyAlpha[ Integer(ka_shift_ctrl_v) ] := 'shift_ctrl_v';
  StrKeyAlpha[ Integer(ka_shift_ctrl_w) ] := 'shift_ctrl_w';
  StrKeyAlpha[ Integer(ka_shift_ctrl_x) ] := 'shift_ctrl_x';
  StrKeyAlpha[ Integer(ka_shift_ctrl_y) ] := 'shift_ctrl_y';
  StrKeyAlpha[ Integer(ka_shift_ctrl_z) ] := 'shift_ctrl_z';
  {Shift+Alt+Alpha} 
  StrKeyAlpha[ Integer(ka_shift_alt_a) ]  := 'shift_alt_a';
  StrKeyAlpha[ Integer(ka_shift_alt_b) ]  := 'shift_alt_b';
  StrKeyAlpha[ Integer(ka_shift_alt_c) ]  := 'shift_alt_c';
  StrKeyAlpha[ Integer(ka_shift_alt_d) ]  := 'shift_alt_d';
  StrKeyAlpha[ Integer(ka_shift_alt_e) ]  := 'shift_alt_e';
  StrKeyAlpha[ Integer(ka_shift_alt_f) ]  := 'shift_alt_f';
  StrKeyAlpha[ Integer(ka_shift_alt_g) ]  := 'shift_alt_g';
  StrKeyAlpha[ Integer(ka_shift_alt_h) ]  := 'shift_alt_h';
  StrKeyAlpha[ Integer(ka_shift_alt_i) ]  := 'shift_alt_i';
  StrKeyAlpha[ Integer(ka_shift_alt_j) ]  := 'shift_alt_j';
  StrKeyAlpha[ Integer(ka_shift_alt_k) ]  := 'shift_alt_k';
  StrKeyAlpha[ Integer(ka_shift_alt_l) ]  := 'shift_alt_l';
  StrKeyAlpha[ Integer(ka_shift_alt_m) ]  := 'shift_alt_m';
  StrKeyAlpha[ Integer(ka_shift_alt_n) ]  := 'shift_alt_n';
  StrKeyAlpha[ Integer(ka_shift_alt_o) ]  := 'shift_alt_o';
  StrKeyAlpha[ Integer(ka_shift_alt_p) ]  := 'shift_alt_p';
  StrKeyAlpha[ Integer(ka_shift_alt_q) ]  := 'shift_alt_q';
  StrKeyAlpha[ Integer(ka_shift_alt_r) ]  := 'shift_alt_r';
  StrKeyAlpha[ Integer(ka_shift_alt_s) ]  := 'shift_alt_s';
  StrKeyAlpha[ Integer(ka_shift_alt_t) ]  := 'shift_alt_t';
  StrKeyAlpha[ Integer(ka_shift_alt_u) ]  := 'shift_alt_u';
  StrKeyAlpha[ Integer(ka_shift_alt_v) ]  := 'shift_alt_v';
  StrKeyAlpha[ Integer(ka_shift_alt_w) ]  := 'shift_alt_w';
  StrKeyAlpha[ Integer(ka_shift_alt_x) ]  := 'shift_alt_x';
  StrKeyAlpha[ Integer(ka_shift_alt_y) ]  := 'shift_alt_y';
  StrKeyAlpha[ Integer(ka_shift_alt_z) ]  := 'shift_alt_z';
  {Shift+Alt+Ctrl+Alpha -- non implémentés vocalement}
  StrKeyAlpha[ Integer(ka_sac_a) ]        := 'shift_alt_ctrl_a';
  StrKeyAlpha[ Integer(ka_sac_b) ]        := 'shift_alt_ctrl_b';
  StrKeyAlpha[ Integer(ka_sac_c) ]        := 'shift_alt_ctrl_c';
  StrKeyAlpha[ Integer(ka_sac_d) ]        := 'shift_alt_ctrl_d';
  StrKeyAlpha[ Integer(ka_sac_e) ]        := 'shift_alt_ctrl_e';
  StrKeyAlpha[ Integer(ka_sac_f) ]        := 'shift_alt_ctrl_f';
  StrKeyAlpha[ Integer(ka_sac_g) ]        := 'shift_alt_ctrl_g';
  StrKeyAlpha[ Integer(ka_sac_h) ]        := 'shift_alt_ctrl_h';
  StrKeyAlpha[ Integer(ka_sac_i) ]        := 'shift_alt_ctrl_i';
  StrKeyAlpha[ Integer(ka_sac_j) ]        := 'shift_alt_ctrl_j';
  StrKeyAlpha[ Integer(ka_sac_k) ]        := 'shift_alt_ctrl_k';
  StrKeyAlpha[ Integer(ka_sac_l) ]        := 'shift_alt_ctrl_l';
  StrKeyAlpha[ Integer(ka_sac_m) ]        := 'shift_alt_ctrl_m';
  StrKeyAlpha[ Integer(ka_sac_n) ]        := 'shift_alt_ctrl_n';
  StrKeyAlpha[ Integer(ka_sac_o) ]        := 'shift_alt_ctrl_o';
  StrKeyAlpha[ Integer(ka_sac_p) ]        := 'shift_alt_ctrl_p';
  StrKeyAlpha[ Integer(ka_sac_q) ]        := 'shift_alt_ctrl_q';
  StrKeyAlpha[ Integer(ka_sac_r) ]        := 'shift_alt_ctrl_r';
  StrKeyAlpha[ Integer(ka_sac_s) ]        := 'shift_alt_ctrl_s';
  StrKeyAlpha[ Integer(ka_sac_t) ]        := 'shift_alt_ctrl_t';
  StrKeyAlpha[ Integer(ka_sac_u) ]        := 'shift_alt_ctrl_u';
  StrKeyAlpha[ Integer(ka_sac_v) ]        := 'shift_alt_ctrl_v';
  StrKeyAlpha[ Integer(ka_sac_w) ]        := 'shift_alt_ctrl_w';
  StrKeyAlpha[ Integer(ka_sac_x) ]        := 'shift_alt_ctrl_x';
  StrKeyAlpha[ Integer(ka_sac_y) ]        := 'shift_alt_ctrl_y';
  StrKeyAlpha[ Integer(ka_sac_z) ]        := 'shift_alt_ctrl_z';
  {Numeriques}
  StrKeyAlpha[ Integer(ka_0) ]            := 'num_0';
  StrKeyAlpha[ Integer(ka_1) ]            := 'num_1';
  StrKeyAlpha[ Integer(ka_2) ]            := 'num_2';
  StrKeyAlpha[ Integer(ka_3) ]            := 'num_3';
  StrKeyAlpha[ Integer(ka_4) ]            := 'num_4';
  StrKeyAlpha[ Integer(ka_5) ]            := 'num_5';
  StrKeyAlpha[ Integer(ka_6) ]            := 'num_6';
  StrKeyAlpha[ Integer(ka_7) ]            := 'num_7';
  StrKeyAlpha[ Integer(ka_8) ]            := 'num_8';
  StrKeyAlpha[ Integer(ka_9) ]            := 'num_9';
  {Shift+Num}
  StrKeyAlpha[ Integer(ka_shift_0) ]      := 'num_shift_0';
  StrKeyAlpha[ Integer(ka_shift_1) ]      := 'num_shift_1';
  StrKeyAlpha[ Integer(ka_shift_2) ]      := 'num_shift_2';
  StrKeyAlpha[ Integer(ka_shift_3) ]      := 'num_shift_3';
  StrKeyAlpha[ Integer(ka_shift_4) ]      := 'num_shift_4';
  StrKeyAlpha[ Integer(ka_shift_5) ]      := 'num_shift_5';
  StrKeyAlpha[ Integer(ka_shift_6) ]      := 'num_shift_6';
  StrKeyAlpha[ Integer(ka_shift_7) ]      := 'num_shift_7';
  StrKeyAlpha[ Integer(ka_shift_8) ]      := 'num_shift_8';
  StrKeyAlpha[ Integer(ka_shift_9) ]      := 'num_shift_9';
  {Alt+Num}
  StrKeyAlpha[ Integer(ka_alt_0) ]        := 'num_alt_0';
  StrKeyAlpha[ Integer(ka_alt_1) ]        := 'num_alt_1';
  StrKeyAlpha[ Integer(ka_alt_2) ]        := 'num_alt_2';
  StrKeyAlpha[ Integer(ka_alt_3) ]        := 'num_alt_3';
  StrKeyAlpha[ Integer(ka_alt_4) ]        := 'num_alt_4';
  StrKeyAlpha[ Integer(ka_alt_5) ]        := 'num_alt_5';
  StrKeyAlpha[ Integer(ka_alt_6) ]        := 'num_alt_6';
  StrKeyAlpha[ Integer(ka_alt_7) ]        := 'num_alt_7';
  StrKeyAlpha[ Integer(ka_alt_8) ]        := 'num_alt_8';
  StrKeyAlpha[ Integer(ka_alt_9) ]        := 'num_alt_9';
  {Ctrl+Num}
  StrKeyAlpha[ Integer(ka_ctrl_0) ]       := 'num_ctrl_0';
  StrKeyAlpha[ Integer(ka_ctrl_1) ]       := 'num_ctrl_1';
  StrKeyAlpha[ Integer(ka_ctrl_2) ]       := 'num_ctrl_2';
  StrKeyAlpha[ Integer(ka_ctrl_3) ]       := 'num_ctrl_3';
  StrKeyAlpha[ Integer(ka_ctrl_4) ]       := 'num_ctrl_4';
  StrKeyAlpha[ Integer(ka_ctrl_5) ]       := 'num_ctrl_5';
  StrKeyAlpha[ Integer(ka_ctrl_6) ]       := 'num_ctrl_6';
  StrKeyAlpha[ Integer(ka_ctrl_7) ]       := 'num_ctrl_7';
  StrKeyAlpha[ Integer(ka_ctrl_8) ]       := 'num_ctrl_8';
  StrKeyAlpha[ Integer(ka_ctrl_9) ]       := 'num_ctrl_9';
  {Shift+Alt+Num}
  StrKeyAlpha[ Integer(ka_shift_alt_0) ]  := 'num_shift_alt_0';
  StrKeyAlpha[ Integer(ka_shift_alt_1) ]  := 'num_shift_alt_1';
  StrKeyAlpha[ Integer(ka_shift_alt_2) ]  := 'num_shift_alt_2';
  StrKeyAlpha[ Integer(ka_shift_alt_3) ]  := 'num_shift_alt_3';
  StrKeyAlpha[ Integer(ka_shift_alt_4) ]  := 'num_shift_alt_4';
  StrKeyAlpha[ Integer(ka_shift_alt_5) ]  := 'num_shift_alt_5';
  StrKeyAlpha[ Integer(ka_shift_alt_6) ]  := 'num_shift_alt_6';
  StrKeyAlpha[ Integer(ka_shift_alt_7) ]  := 'num_shift_alt_7';
  StrKeyAlpha[ Integer(ka_shift_alt_8) ]  := 'num_shift_alt_8';
  StrKeyAlpha[ Integer(ka_shift_alt_9) ]  := 'num_shift_alt_9';
  {Shift+Ctrl+Num}
  StrKeyAlpha[ Integer(ka_shift_ctrl_0) ] := 'num_shift_ctrl_0';
  StrKeyAlpha[ Integer(ka_shift_ctrl_1) ] := 'num_shift_ctrl_1';
  StrKeyAlpha[ Integer(ka_shift_ctrl_2) ] := 'num_shift_ctrl_2';
  StrKeyAlpha[ Integer(ka_shift_ctrl_3) ] := 'num_shift_ctrl_3';
  StrKeyAlpha[ Integer(ka_shift_ctrl_4) ] := 'num_shift_ctrl_4';
  StrKeyAlpha[ Integer(ka_shift_ctrl_5) ] := 'num_shift_ctrl_5';
  StrKeyAlpha[ Integer(ka_shift_ctrl_6) ] := 'num_shift_ctrl_6';
  StrKeyAlpha[ Integer(ka_shift_ctrl_7) ] := 'num_shift_ctrl_7';
  StrKeyAlpha[ Integer(ka_shift_ctrl_8) ] := 'num_shift_ctrl_8';
  StrKeyAlpha[ Integer(ka_shift_ctrl_9) ] := 'num_shift_ctrl_9';
  {Alt+Ctrl+Num}
  StrKeyAlpha[ Integer(ka_alt_ctrl_0) ]   := 'num_alt_ctrl_0';
  StrKeyAlpha[ Integer(ka_alt_ctrl_1) ]   := 'num_alt_ctrl_1';
  StrKeyAlpha[ Integer(ka_alt_ctrl_2) ]   := 'num_alt_ctrl_2';
  StrKeyAlpha[ Integer(ka_alt_ctrl_3) ]   := 'num_alt_ctrl_3';
  StrKeyAlpha[ Integer(ka_alt_ctrl_4) ]   := 'num_alt_ctrl_4';
  StrKeyAlpha[ Integer(ka_alt_ctrl_5) ]   := 'num_alt_ctrl_5';
  StrKeyAlpha[ Integer(ka_alt_ctrl_6) ]   := 'num_alt_ctrl_6';
  StrKeyAlpha[ Integer(ka_alt_ctrl_7) ]   := 'num_alt_ctrl_7';
  StrKeyAlpha[ Integer(ka_alt_ctrl_8) ]   := 'num_alt_ctrl_8';
  StrKeyAlpha[ Integer(ka_alt_ctrl_9) ]   := 'num_alt_ctrl_9';
  {Shift+Alt+Ctrl+Num -- vocalement non implémenté}
  StrKeyAlpha[ Integer(ka_sac_0) ]        := 'num_sac_0';
  StrKeyAlpha[ Integer(ka_sac_1) ]        := 'num_sac_1';
  StrKeyAlpha[ Integer(ka_sac_2) ]        := 'num_sac_2';
  StrKeyAlpha[ Integer(ka_sac_3) ]        := 'num_sac_3';
  StrKeyAlpha[ Integer(ka_sac_4) ]        := 'num_sac_4';
  StrKeyAlpha[ Integer(ka_sac_5) ]        := 'num_sac_5';
  StrKeyAlpha[ Integer(ka_sac_6) ]        := 'num_sac_6';
  StrKeyAlpha[ Integer(ka_sac_7) ]        := 'num_sac_7';
  StrKeyAlpha[ Integer(ka_sac_8) ]        := 'num_sac_8';
  StrKeyAlpha[ Integer(ka_sac_9) ]        := 'num_sac_9';
end;


procedure StrKeyFxInitialize;
begin
  {Fx}
  StrKeyFx[ Integer(kx_f1) ]           := 'f1';
  StrKeyFx[ Integer(kx_f2) ]           := 'f2';
  StrKeyFx[ Integer(kx_f3) ]           := 'f3';
  StrKeyFx[ Integer(kx_f4) ]           := 'f4';
  StrKeyFx[ Integer(kx_f5) ]           := 'f5';
  StrKeyFx[ Integer(kx_f6) ]           := 'f6';
  StrKeyFx[ Integer(kx_f7) ]           := 'f7';
  StrKeyFx[ Integer(kx_f8) ]           := 'f8';
  StrKeyFx[ Integer(kx_f9) ]           := 'f9';
  StrKeyFx[ Integer(kx_f10) ]          := 'f10';
  StrKeyFx[ Integer(kx_f11) ]          := 'f11';
  StrKeyFx[ Integer(kx_f12) ]          := 'f12';
  StrKeyFx[ Integer(kx_f13) ]          := 'f13';
  StrKeyFx[ Integer(kx_f14) ]          := 'f14';
  StrKeyFx[ Integer(kx_f15) ]          := 'f15';
  StrKeyFx[ Integer(kx_f16) ]          := 'f16';
  StrKeyFx[ Integer(kx_f17) ]          := 'f17';
  StrKeyFx[ Integer(kx_f18) ]          := 'f18';
  StrKeyFx[ Integer(kx_f19) ]          := 'f19';
  StrKeyFx[ Integer(kx_f20) ]          := 'f20';
  StrKeyFx[ Integer(kx_f21) ]          := 'f21';
  StrKeyFx[ Integer(kx_f22) ]          := 'f22';
  StrKeyFx[ Integer(kx_f23) ]          := 'f23';
  StrKeyFx[ Integer(kx_f24) ]          := 'f24';
  {Shift+Fx}
  StrKeyFx[ Integer(kx_shift_f1) ]     := 'shift_f1';
  StrKeyFx[ Integer(kx_shift_f2) ]     := 'shift_f2';
  StrKeyFx[ Integer(kx_shift_f3) ]     := 'shift_f3';
  StrKeyFx[ Integer(kx_shift_f4) ]     := 'shift_f4';
  StrKeyFx[ Integer(kx_shift_f5) ]     := 'shift_f5';
  StrKeyFx[ Integer(kx_shift_f6) ]     := 'shift_f6';
  StrKeyFx[ Integer(kx_shift_f7) ]     := 'shift_f7';
  StrKeyFx[ Integer(kx_shift_f8) ]     := 'shift_f8';
  StrKeyFx[ Integer(kx_shift_f9) ]     := 'shift_f9';
  StrKeyFx[ Integer(kx_shift_f10) ]    := 'shift_f10';
  StrKeyFx[ Integer(kx_shift_f11) ]    := 'shift_f11';
  StrKeyFx[ Integer(kx_shift_f12) ]    := 'shift_f11';
  StrKeyFx[ Integer(kx_shift_f13) ]    := 'shift_f13';
  StrKeyFx[ Integer(kx_shift_f14) ]    := 'shift_f14';
  StrKeyFx[ Integer(kx_shift_f15) ]    := 'shift_f15';
  StrKeyFx[ Integer(kx_shift_f16) ]    := 'shift_f16';
  StrKeyFx[ Integer(kx_shift_f17) ]    := 'shift_f17';
  StrKeyFx[ Integer(kx_shift_f18) ]    := 'shift_f18';
  StrKeyFx[ Integer(kx_shift_f19) ]    := 'shift_f19';
  StrKeyFx[ Integer(kx_shift_f20) ]    := 'shift_f20';
  StrKeyFx[ Integer(kx_shift_f21) ]    := 'shift_f21';
  StrKeyFx[ Integer(kx_shift_f22) ]    := 'shift_f22';
  StrKeyFx[ Integer(kx_shift_f23) ]    := 'shift_f23';
  StrKeyFx[ Integer(kx_shift_f24) ]    := 'shift_f24';
  {Alt+Fx}  
  StrKeyFx[ Integer(kx_alt_f1) ]       := 'alt_f1';
  StrKeyFx[ Integer(kx_alt_f2) ]       := 'alt_f2';
  StrKeyFx[ Integer(kx_alt_f3) ]       := 'alt_f3';
  StrKeyFx[ Integer(kx_alt_f4) ]       := 'alt_f4';
  StrKeyFx[ Integer(kx_alt_f5) ]       := 'alt_f5';
  StrKeyFx[ Integer(kx_alt_f6) ]       := 'alt_f6';
  StrKeyFx[ Integer(kx_alt_f7) ]       := 'alt_f7';
  StrKeyFx[ Integer(kx_alt_f8) ]       := 'alt_f8';
  StrKeyFx[ Integer(kx_alt_f9) ]       := 'alt_f9';
  StrKeyFx[ Integer(kx_alt_f10) ]      := 'alt_f10';
  StrKeyFx[ Integer(kx_alt_f11) ]      := 'alt_f11';
  StrKeyFx[ Integer(kx_alt_f12) ]      := 'alt_f12';
  StrKeyFx[ Integer(kx_alt_f13) ]      := 'alt_f13';
  StrKeyFx[ Integer(kx_alt_f14) ]      := 'alt_f14';
  StrKeyFx[ Integer(kx_alt_f15) ]      := 'alt_f15';
  StrKeyFx[ Integer(kx_alt_f16) ]      := 'alt_f16';
  StrKeyFx[ Integer(kx_alt_f17) ]      := 'alt_f17';
  StrKeyFx[ Integer(kx_alt_f18) ]      := 'alt_f18';
  StrKeyFx[ Integer(kx_alt_f19) ]      := 'alt_f19';
  StrKeyFx[ Integer(kx_alt_f20) ]      := 'alt_f20';
  StrKeyFx[ Integer(kx_alt_f21) ]      := 'alt_f21';
  StrKeyFx[ Integer(kx_alt_f22) ]      := 'alt_f22';
  StrKeyFx[ Integer(kx_alt_f23) ]      := 'alt_f23';
  StrKeyFx[ Integer(kx_alt_f24) ]      := 'alt_f24';
  {Ctrl+Fx}
  StrKeyFx[ Integer(kx_ctrl_f1) ]      := 'ctrl_f1';
  StrKeyFx[ Integer(kx_ctrl_f2) ]      := 'ctrl_f2';
  StrKeyFx[ Integer(kx_ctrl_f3) ]      := 'ctrl_f3';
  StrKeyFx[ Integer(kx_ctrl_f4) ]      := 'ctrl_f4';
  StrKeyFx[ Integer(kx_ctrl_f5) ]      := 'ctrl_f5';
  StrKeyFx[ Integer(kx_ctrl_f6) ]      := 'ctrl_f6';
  StrKeyFx[ Integer(kx_ctrl_f7) ]      := 'ctrl_f7';
  StrKeyFx[ Integer(kx_ctrl_f8) ]      := 'ctrl_f8';
  StrKeyFx[ Integer(kx_ctrl_f9) ]      := 'ctrl_f9';
  StrKeyFx[ Integer(kx_ctrl_f10) ]     := 'ctrl_f10';
  StrKeyFx[ Integer(kx_ctrl_f11) ]     := 'ctrl_f11';
  StrKeyFx[ Integer(kx_ctrl_f12) ]     := 'ctrl_f12';
  StrKeyFx[ Integer(kx_ctrl_f13) ]     := 'ctrl_f13';
  StrKeyFx[ Integer(kx_ctrl_f14) ]     := 'ctrl_f14';
  StrKeyFx[ Integer(kx_ctrl_f15) ]     := 'ctrl_f15';
  StrKeyFx[ Integer(kx_ctrl_f16) ]     := 'ctrl_f16';
  StrKeyFx[ Integer(kx_ctrl_f17) ]     := 'ctrl_f17';
  StrKeyFx[ Integer(kx_ctrl_f18) ]     := 'ctrl_f18';
  StrKeyFx[ Integer(kx_ctrl_f19) ]     := 'ctrl_f19';
  StrKeyFx[ Integer(kx_ctrl_f20) ]     := 'ctrl_f20';
  StrKeyFx[ Integer(kx_ctrl_f21) ]     := 'ctrl_f21';
  StrKeyFx[ Integer(kx_ctrl_f22) ]     := 'ctrl_f22';
  StrKeyFx[ Integer(kx_ctrl_f23) ]     := 'ctrl_f23';
  StrKeyFx[ Integer(kx_ctrl_f24) ]     := 'ctrl_f24';
  {Shift+Alt+Fx}
  StrKeyFx[ Integer(kx_shalt_f1) ]     := 'shift_alt_f1';
  StrKeyFx[ Integer(kx_shalt_f2) ]     := 'shift_alt_f2';
  StrKeyFx[ Integer(kx_shalt_f3) ]     := 'shift_alt_f3';
  StrKeyFx[ Integer(kx_shalt_f4) ]     := 'shift_alt_f4';
  StrKeyFx[ Integer(kx_shalt_f5) ]     := 'shift_alt_f5';
  StrKeyFx[ Integer(kx_shalt_f6) ]     := 'shift_alt_f6';
  StrKeyFx[ Integer(kx_shalt_f7) ]     := 'shift_alt_f7';
  StrKeyFx[ Integer(kx_shalt_f8) ]     := 'shift_alt_f8';
  StrKeyFx[ Integer(kx_shalt_f9) ]     := 'shift_alt_f9';
  StrKeyFx[ Integer(kx_shalt_f10) ]    := 'shift_alt_f10';
  StrKeyFx[ Integer(kx_shalt_f11) ]    := 'shift_alt_f11';
  StrKeyFx[ Integer(kx_shalt_f12) ]    := 'shift_alt_f12';
  StrKeyFx[ Integer(kx_shalt_f13) ]    := 'shift_alt_f13';
  StrKeyFx[ Integer(kx_shalt_f14) ]    := 'shift_alt_f14';
  StrKeyFx[ Integer(kx_shalt_f15) ]    := 'shift_alt_f15';
  StrKeyFx[ Integer(kx_shalt_f16) ]    := 'shift_alt_f16';
  StrKeyFx[ Integer(kx_shalt_f17) ]    := 'shift_alt_f17';
  StrKeyFx[ Integer(kx_shalt_f18) ]    := 'shift_alt_f18';
  StrKeyFx[ Integer(kx_shalt_f19) ]    := 'shift_alt_f19';
  StrKeyFx[ Integer(kx_shalt_f20) ]    := 'shift_alt_f20';
  StrKeyFx[ Integer(kx_shalt_f21) ]    := 'shift_alt_f21';
  StrKeyFx[ Integer(kx_shalt_f22) ]    := 'shift_alt_f22';
  StrKeyFx[ Integer(kx_shalt_f23) ]    := 'shift_alt_f23';
  StrKeyFx[ Integer(kx_shalt_f24) ]    := 'shift_alt_f24';
  {Ctrl+Alt+Fx}
  StrKeyFx[ Integer(kx_ctalt_f1) ]     := 'ctrl_alt_f1';
  StrKeyFx[ Integer(kx_ctalt_f2) ]     := 'ctrl_alt_f2';
  StrKeyFx[ Integer(kx_ctalt_f3) ]     := 'ctrl_alt_f3';
  StrKeyFx[ Integer(kx_ctalt_f4) ]     := 'ctrl_alt_f4';
  StrKeyFx[ Integer(kx_ctalt_f5) ]     := 'ctrl_alt_f5';
  StrKeyFx[ Integer(kx_ctalt_f6) ]     := 'ctrl_alt_f6';
  StrKeyFx[ Integer(kx_ctalt_f7) ]     := 'ctrl_alt_f7';
  StrKeyFx[ Integer(kx_ctalt_f8) ]     := 'ctrl_alt_f8';
  StrKeyFx[ Integer(kx_ctalt_f9) ]     := 'ctrl_alt_f9';
  StrKeyFx[ Integer(kx_ctalt_f10) ]    := 'ctrl_alt_f10';
  StrKeyFx[ Integer(kx_ctalt_f11) ]    := 'ctrl_alt_f11';
  StrKeyFx[ Integer(kx_ctalt_f12) ]    := 'ctrl_alt_f12';
  StrKeyFx[ Integer(kx_ctalt_f13) ]    := 'ctrl_alt_f13';
  StrKeyFx[ Integer(kx_ctalt_f14) ]    := 'ctrl_alt_f14';
  StrKeyFx[ Integer(kx_ctalt_f15) ]    := 'ctrl_alt_f15';
  StrKeyFx[ Integer(kx_ctalt_f16) ]    := 'ctrl_alt_f16';
  StrKeyFx[ Integer(kx_ctalt_f17) ]    := 'ctrl_alt_f17';
  StrKeyFx[ Integer(kx_ctalt_f18) ]    := 'ctrl_alt_f18';
  StrKeyFx[ Integer(kx_ctalt_f19) ]    := 'ctrl_alt_f19';
  StrKeyFx[ Integer(kx_ctalt_f20) ]    := 'ctrl_alt_f20';
  StrKeyFx[ Integer(kx_ctalt_f21) ]    := 'ctrl_alt_f21';
  StrKeyFx[ Integer(kx_ctalt_f22) ]    := 'ctrl_alt_f22';
  StrKeyFx[ Integer(kx_ctalt_f23) ]    := 'ctrl_alt_f23';
  StrKeyFx[ Integer(kx_ctalt_f24) ]    := 'ctrl_alt_f24';
  {Shift+Ctrl+Fx}
  StrKeyFx[ Integer(kx_shctrl_f1) ]    := 'shift_ctrl_f1';
  StrKeyFx[ Integer(kx_shctrl_f2) ]    := 'shift_ctrl_f2';
  StrKeyFx[ Integer(kx_shctrl_f3) ]    := 'shift_ctrl_f3';
  StrKeyFx[ Integer(kx_shctrl_f4) ]    := 'shift_ctrl_f4';
  StrKeyFx[ Integer(kx_shctrl_f5) ]    := 'shift_ctrl_f5';
  StrKeyFx[ Integer(kx_shctrl_f6) ]    := 'shift_ctrl_f6';
  StrKeyFx[ Integer(kx_shctrl_f7) ]    := 'shift_ctrl_f7';
  StrKeyFx[ Integer(kx_shctrl_f8) ]    := 'shift_ctrl_f8';
  StrKeyFx[ Integer(kx_shctrl_f9) ]    := 'shift_ctrl_f9';
  StrKeyFx[ Integer(kx_shctrl_f10) ]   := 'shift_ctrl_f10';
  StrKeyFx[ Integer(kx_shctrl_f11) ]   := 'shift_ctrl_f11';
  StrKeyFx[ Integer(kx_shctrl_f12) ]   := 'shift_ctrl_f12';
  StrKeyFx[ Integer(kx_shctrl_f13) ]   := 'shift_ctrl_f13';
  StrKeyFx[ Integer(kx_shctrl_f14) ]   := 'shift_ctrl_f14';
  StrKeyFx[ Integer(kx_shctrl_f15) ]   := 'shift_ctrl_f15';
  StrKeyFx[ Integer(kx_shctrl_f16) ]   := 'shift_ctrl_f16';
  StrKeyFx[ Integer(kx_shctrl_f17) ]   := 'shift_ctrl_f17';
  StrKeyFx[ Integer(kx_shctrl_f18) ]   := 'shift_ctrl_f18';
  StrKeyFx[ Integer(kx_shctrl_f19) ]   := 'shift_ctrl_f19';
  StrKeyFx[ Integer(kx_shctrl_f20) ]   := 'shift_ctrl_f20';
  StrKeyFx[ Integer(kx_shctrl_f21) ]   := 'shift_ctrl_f21';
  StrKeyFx[ Integer(kx_shctrl_f22) ]   := 'shift_ctrl_f22';
  StrKeyFx[ Integer(kx_shctrl_f23) ]   := 'shift_ctrl_f23';
  StrKeyFx[ Integer(kx_shctrl_f24) ]   := 'shift_ctrl_f24';
  {Shift+Alt+Ctrl+Fx -- non implémentés vocalement}
  StrKeyFx[ Integer(kx_sac_f1) ]       := 'shift_alt_ctrl_f1';
  StrKeyFx[ Integer(kx_sac_f2) ]       := 'shift_alt_ctrl_f2';
  StrKeyFx[ Integer(kx_sac_f3) ]       := 'shift_alt_ctrl_f3';
  StrKeyFx[ Integer(kx_sac_f4) ]       := 'shift_alt_ctrl_f4';
  StrKeyFx[ Integer(kx_sac_f5) ]       := 'shift_alt_ctrl_f5';
  StrKeyFx[ Integer(kx_sac_f6) ]       := 'shift_alt_ctrl_f6';
  StrKeyFx[ Integer(kx_sac_f7) ]       := 'shift_alt_ctrl_f7';
  StrKeyFx[ Integer(kx_sac_f8) ]       := 'shift_alt_ctrl_f8';
  StrKeyFx[ Integer(kx_sac_f9) ]       := 'shift_alt_ctrl_f9';
  StrKeyFx[ Integer(kx_sac_f10) ]      := 'shift_alt_ctrl_f10';
  StrKeyFx[ Integer(kx_sac_f11) ]      := 'shift_alt_ctrl_f11';
  StrKeyFx[ Integer(kx_sac_f12) ]      := 'shift_alt_ctrl_f12';
  StrKeyFx[ Integer(kx_sac_f13) ]      := 'shift_alt_ctrl_f13';
  StrKeyFx[ Integer(kx_sac_f14) ]      := 'shift_alt_ctrl_f14';
  StrKeyFx[ Integer(kx_sac_f15) ]      := 'shift_alt_ctrl_f15';
  StrKeyFx[ Integer(kx_sac_f16) ]      := 'shift_alt_ctrl_f16';
  StrKeyFx[ Integer(kx_sac_f17) ]      := 'shift_alt_ctrl_f17';
  StrKeyFx[ Integer(kx_sac_f18) ]      := 'shift_alt_ctrl_f18';
  StrKeyFx[ Integer(kx_sac_f19) ]      := 'shift_alt_ctrl_f19';
  StrKeyFx[ Integer(kx_sac_f20) ]      := 'shift_alt_ctrl_f20';
  StrKeyFx[ Integer(kx_sac_f21) ]      := 'shift_alt_ctrl_f21';
  StrKeyFx[ Integer(kx_sac_f22) ]      := 'shift_alt_ctrl_f22';
  StrKeyFx[ Integer(kx_sac_f23) ]      := 'shift_alt_ctrl_f23';
  StrKeyFx[ Integer(kx_sac_f24) ]      := 'shift_alt_ctrl_f24';
end;



{ --- Routines for managment --- }

function StrKeyToIdent(const Value: string): TKeyIdent;
var
  index: Integer;
begin
  index := IndexStr(AnsiLowerCase(Value), StrKeyIdent);
  case index of
    -1 : Result := ki_none
    else Result := TKeyIdent(index)
  end
end;

function StrKeyToFunction(const Value: string): TKeyEditor;
var
  index: Integer;
begin
  index := IndexStr(AnsiLowerCase(Value), StrKeyEditor);
  case index of
    -1 : Result := kf_none
    else Result := TKeyEditor(index)
  end
end;

function StrKeyToKeyFx(const Value: string): TKeyFx;
var
  index: Integer;
begin
  index := IndexStr(AnsiLowerCase(Value), StrKeyFx);
  case index of
    -1 : Result := kx_none
    else Result := TKeyFx(index)
  end
end;

function StrKeyToAlpha(const Value: string): TKeyAlpha;
var
  index: Integer;
begin
  index := IndexStr(AnsiLowerCase(Value), StrKeyAlpha);
  case index of
    -1 : Result := ka_none
    else Result := TKeyAlpha(index)
  end
end;

function StrKeyToDir(const Value: string): TKeydir;
var
  index: Integer;
begin
  index := IndexStr(AnsiLowerCase(Value), StrKeyDir);
  case index of
    -1 : Result := kd_none
    else Result := TKeyDir(index)
  end
end;

function StrKeyToWinApp(const Value: string): TKeyWinApp;
var
  index: Integer;
begin
  index := IndexStr(AnsiLowerCase(Value), StrKeyWinApp);
  case index of
    -1 : Result := kw_none
    else Result := TKeyWinApp(index)
  end
end;

function StrKeyToElite(const Value: string): TKeyElite;
var
  index: Integer;
begin
  index := IndexStr(AnsiLowerCase(Value), StrKeyElite);
  case index of
    -1 : Result := ke_none
    else Result := TKeyElite(index)
  end
end;

function StrKeyToTimer(const Value: string): TKeyTimer;
var
  index: Integer;
begin
  index := IndexStr(AnsiLowerCase(Value), StrKeyTimer);
  case index of
    -1 : Result := kt_none
    else Result := TKeyTimer(index)
  end
end;

function LowerCharCode(const KeyIdent: TKeyIdent):string; overload;
begin
  Result := KeyCodes[Integer(KeyIdent)].LowerCode
end;

function LowerCharCode(const StrKey: string):string; overload;
begin
  Result := LowerCharCode( StrKeyToIdent(StrKey) )
end;

function UpperCharCode(const KeyIdent: TKeyIdent):string; overload;
begin
  Result := KeyCodes[Integer(KeyIdent)].UpperCode
end;

function UpperCharCode(const StrKey: string):string; overload;
begin
  Result := UpperCharCode( StrKeyToIdent(StrKey) )
end;

function StrKeyToCode(const Value: string): string;
begin
  if CapsLocKEnable
    then Result := UpperCharCode(Value)
    else Result := LowerCharCode(Value)
end;

function KeyIdentifier(const Value: string): TKeyKind;
begin
  AltCheck(Value);  //relache si besoin la touche VL_LMENU
  Result := kk_touche;
  if StrKeyToIdent(Value)    <> ki_none then Result := kk_codetouche
    else
  if StrKeyToFunction(Value) <> kf_none then Result := kk_function
    else
  if StrKeyToKeyFx(Value)    <> kx_none then Result := kk_keyfx
    else
  if StrKeyToAlpha(Value)    <> ka_none then Result := kk_keyalpha
    else
  if StrKeyToDir(Value)      <> kd_none then Result := kk_keydir
    else
  if StrKeyToWinApp(Value)   <> kw_none then Result := kk_winapp
    else
  if StrKeyToElite(Value)    <> ke_none then Result := kk_elite
    else
  if StrKeyToTimer(Value)    <> kt_none then Result := kk_timer
end;

{ TCustomKeyEditors }

procedure TCustomKeyEditors.Alt;
begin
  SendKey(VK_MENU,  10, [])
end;

procedure TCustomKeyEditors.AltRelease;
begin
  AltRelease_
end;

procedure TCustomKeyEditors.AltTab;
begin
  keybd_event(VK_LMENU, MapVirtualKey(VK_LMENU, 0), 0, 0);
  keybd_event(VK_TAB,   MapVirtualKey(VK_TAB,   0), 0, 0);

  keybd_event(VK_TAB,   MapVirtualKey(VK_TAB,   0), KEYEVENTF_KEYUP, 0);
  KeyWrite(ParamKey, 'Alt_Tab', True);
end;

procedure TCustomKeyEditors.Clear;
begin
  SendKey(VkKeyScan('a'), 10, [ss_lctrl]);
  SendKey(VK_BACK,        10, [])
end;

procedure TCustomKeyEditors.Copy;
begin
  SendKey(VkKeyScan('c'), 10, [ss_lctrl])
end;

procedure TCustomKeyEditors.Ctrl;
begin
  SendKey(VK_CONTROL, 10, [])
end;

procedure TCustomKeyEditors.Cut;
begin
  SendKey(VkKeyScan('x'), 10, [ss_lctrl])
end;

procedure TCustomKeyEditors.DoAgain;
var
  Command: string;
begin
  Command := ReadLastCommand;
  if AllowedAgain(Command) then ProcessOnKey(Command)
end;

procedure TCustomKeyEditors.DoAgain(const Count: Integer);
var
  i : Integer;
begin
  for i := 1 to Count do DoAgain
end;

function TCustomKeyEditors.Enabled: Boolean;
var
  Default: Boolean;
begin
  Default := KeyReadBoolean(IniKey, 'pendefault', False);
  Result  := KeyReadBoolean(AppKey, 'pen',        Default);
end;

procedure TCustomKeyEditors.Execute_(const Value: string);
begin
  SetCurrent(Value);
  case FCurrent of
    {main validations}
    kf_penon         : PenOn;
    kf_penoff        : PenOff;
    kf_alt           : Alt;
    kf_ctrl          : Ctrl;
    kf_altrelease    : AltRelease;
    kf_activsoft     : AltTab;
    kf_again         : DoAgain;
    kf_again2        : DoAgain(2);
    kf_again3        : DoAgain(3);
    kf_again5        : DoAgain(5);
    {main commands}
    kf_home          : ToHome;
    kf_end           : ToEnd;
    kf_selectall     : SelectAll;
    kf_copy          : Copy;
    kf_paste         : Paste;
    kf_cut           : Cut;
    kf_unselect      : UnSelect;
    kf_clear         : Clear;
    kf_tophome       : ToTopHome;
    kf_bottomend     : ToBottomEnd;
    kf_seltoend      : SelToEnd;
    kf_selalltoend   : SelAllToEnd;
    kf_seltohome     : SelToHome;
    kf_selalltohome  : SelAllToHome;
    kf_selhomepage   : SelHomePage;
    kf_selendpage    : SelEndPage;
    kf_selword       : SelWord;
    kf_wordnext      : WordNext;
    kf_wordprior     : WordPrior;
    {Search box}
    kf_search        : Search;
    kf_selsearch     : SelSearch;
    kf_searchnext    : SearchNext;
    {App menu}
    kf_menuopen      : MenuOpen;
    kf_menuclose     : MenuClose;
  end
end;

function TCustomKeyEditors.GetText: string;
begin
  Result := StrArrayToStr( StrKeyEditor )
end;

procedure TCustomKeyEditors.MenuClose;
begin
  SendKey(VK_ESCAPE, 10, []);
  SendKey(VK_ESCAPE, 10, [])
end;

procedure TCustomKeyEditors.MenuOpen;
begin
  SendKey(VK_MENU,   10, []);
  SendKey(VK_RETURN, 10, [])
end;

procedure TCustomKeyEditors.Paste;
begin
  SendKey(VkKeyScan('v'), 10, [ss_lctrl])
end;

procedure TCustomKeyEditors.PenOff;
begin
  KeyWrite(AppKey, 'pen', False)
end;

procedure TCustomKeyEditors.PenOn;
begin
  KeyWrite(AppKey, 'pen', True)
end;

procedure TCustomKeyEditors.Search;
begin
  SendKey(VkKeyScan('F'), 10, [ss_lctrl])
end;

procedure TCustomKeyEditors.SearchNext;
begin
  SendKey(VK_F3,    10, [])
end;

procedure TCustomKeyEditors.SelAllToEnd;
begin
  SendKey(VK_END,   10, [ss_lctrl, ss_lshift])
end;

procedure TCustomKeyEditors.SelAllToHome;
begin
  SendKey(VK_HOME,  10, [ss_lctrl, ss_lshift])
end;

procedure TCustomKeyEditors.SelectAll;
begin
  SendKey(VkKeyScan('a'), 10, [ss_lctrl])
end;

procedure TCustomKeyEditors.SelEndPage;
begin
  SendKey(VK_NEXT,  10, [ss_lshift]);
end;

procedure TCustomKeyEditors.SelHomePage;
begin
  SendKey(VK_PRIOR, 10, [ss_lshift])
end;

procedure TCustomKeyEditors.SelSearch;
begin
  SendKey(VkKeyScan('c'),     10, [ss_lctrl]);
  SendKey(VkKeyScan('F'),    300, [ss_lctrl]);
  SendKey(VkKeyScan('v'),     10, [ss_lctrl]);
end;

procedure TCustomKeyEditors.SelToEnd;
begin
  SendKey(VK_END,   10, [ss_lshift])
end;

procedure TCustomKeyEditors.SelToHome;
begin
  SendKey(VK_HOME,  10, [ss_lshift])
end;

procedure TCustomKeyEditors.SelWord;
begin
  SendKey(VK_RIGHT, 10, [ss_lctrl]);
  SendKey(VK_LEFT,  10, [ss_lshift, ss_lctrl])
end;

procedure TCustomKeyEditors.SetCurrent(const Value: string);
begin
  FCurrent := StrKeyToFunction( Value )
end;

procedure TCustomKeyEditors.ToBottomEnd;
begin
  SendKey(VK_END,   10, [ss_lctrl])
end;

procedure TCustomKeyEditors.ToEnd;
begin
  SendKey(VK_END,   10, [])
end;

procedure TCustomKeyEditors.ToHome;
begin
  SendKey(VK_HOME,  10, [])
end;

procedure TCustomKeyEditors.ToLeft;
begin
  TKeyBoardDir.Execute('left')
end;

procedure TCustomKeyEditors.ToRight;
begin
  TKeyBoardDir.Execute('right')
end;

procedure TCustomKeyEditors.ToTopHome;
begin
  SendKey(VK_HOME,  10, [ss_lctrl])
end;

procedure TCustomKeyEditors.UnSelect;
begin
  ToLeft;  ToRight;
end;

procedure TCustomKeyEditors.WordNext;
begin
  SendKey(VK_RIGHT, 10, [ss_lctrl]);
end;

procedure TCustomKeyEditors.WordPrior;
begin
  SendKey(VK_LEFT,  10, [ss_lctrl]);
end;

{ TKeyEditors }

class procedure TKeyEditors.Execute(const Value: string);
begin
  if Assigned(KeyEditors) then with KeyEditors do Execute_(Value)
end;

class procedure TKeyEditors.Finalize;
begin
  FreeAndNil( KeyEditors )
end;

class function TKeyEditors.Initialize: TKeyEditors;
begin
  KeyEditors := TKeyEditors.Create(Application.MainForm);
  Result     := KeyEditors;
end;

class function TKeyEditors.Text: string;
begin
  Result := EmptyStr;
  if Assigned(KeyEditors) then with KeyEditors do Result := Text_
end;

class function TKeyEditors.Writable: Boolean;
begin
  if Assigned(KeyEditors) then with KeyEditors do Result := Enabled
    else Result := False
end;

{ TCustomKeyFx }

procedure TCustomKeyFx.Execute_(const Value: string);
begin
  SetCurrent(Value);
  case FCurrent of
    {Fx keys}
    kx_f1..kx_f24                  : FKeys( FCurrent, []);
    {Shift+Fx keys}
    kx_shift_f1..kx_shift_f24      : FKeys( FCurrent, [ss_lshift]);
    {Alt+Fx keys}
    kx_alt_f1..kx_alt_f24          : FKeys( FCurrent, [ss_lalt]);
    {Ctrl+Fx keys}
    kx_ctrl_f1..kx_ctrl_f24        : FKeys( FCurrent, [ss_lctrl]);
    {Shift+Alt+Fx keys }
    kx_shalt_f1..kx_shalt_f24      : FKeys( FCurrent, [ss_lshift, ss_lalt]);
    {Ctrl+Alt+Fx keys }
    kx_ctalt_f1..kx_ctalt_f24      : FKeys( FCurrent, [ss_lctrl, ss_lalt]);
    {Shift+Ctrl+Fx Keys}
    kx_shctrl_f1..kx_shctrl_f24    : FKeys( FCurrent, [ss_lshift, ss_lctrl]);
    {Shift+Alt+Ctrl+Fx Keys -- non implémentés vocalement}
    kx_sac_f1..kx_sac_f24          : FKeys( FCurrent, [ss_lshift, ss_lalt, ss_lctrl]);
  end
end;

procedure TCustomKeyFx.FKeys(const Value: TKeyFx; Specials: TSpecials);
begin
  case ModuloOfFunc(Value) of
    kx_f1   : SendKey(VK_F1,  10, Specials);
    kx_f2   : SendKey(VK_F2,  10, Specials);
    kx_f3   : SendKey(VK_F3,  10, Specials);
    kx_f4   : SendKey(VK_F4,  10, Specials);
    kx_f5   : SendKey(VK_F5,  10, Specials);
    kx_f6   : SendKey(VK_F6,  10, Specials);
    kx_f7   : SendKey(VK_F7,  10, Specials);
    kx_f8   : SendKey(VK_F8,  10, Specials);
    kx_f9   : SendKey(VK_F9,  10, Specials);
    kx_f10  : SendKey(VK_F10, 10, Specials);
    kx_f11  : SendKey(VK_F11, 10, Specials);
    kx_f12  : SendKey(VK_F12, 10, Specials);
    kx_f13  : SendKey(VK_F13, 10, Specials);
    kx_f14  : SendKey(VK_F14, 10, Specials);
    kx_f15  : SendKey(VK_F15, 10, Specials);
    kx_f16  : SendKey(VK_F16, 10, Specials);
    kx_f17  : SendKey(VK_F17, 10, Specials);
    kx_f18  : SendKey(VK_F18, 10, Specials);
    kx_f19  : SendKey(VK_F19, 10, Specials);
    kx_f20  : SendKey(VK_F20, 10, Specials);
    kx_f21  : SendKey(VK_F21, 10, Specials);
    kx_f22  : SendKey(VK_F22, 10, Specials);
    kx_f23  : SendKey(VK_F23, 10, Specials);
    kx_f24  : SendKey(VK_F24, 10, Specials);
  end
end;

function TCustomKeyFx.GetText: string;
begin
  Result := StrArrayToStr( StrKeyFx )
end;

procedure TCustomKeyFx.SetCurrent(const Value: string);
begin
  FCurrent := StrKeyToKeyFx( Value )
end;

{ TKeyBoardFx }

class procedure TKeyBoardFx.Execute(const Value: string);
begin
  if Assigned(KeyBoardFx) then with KeyBoardFx do Execute_(Value);
end;

class procedure TKeyBoardFx.Finalize;
begin
  FreeAndNil( KeyBoardFx )
end;

class function TKeyBoardFx.Initialize: TKeyBoardFx;
begin
  KeyBoardFx := TKeyBoardFx.Create(Application.MainForm);
  Result     := KeyBoardFx
end;

class function TKeyBoardFx.Text: string;
begin
  Result := EmptyStr;
  if Assigned(KeyBoardFx) then with KeyBoardFx do Result := Text_
end;

class function TKeyBoardFx.Writable: Boolean;
begin
  Result := TKeyEditors.Writable
end;

{ TCustomKeyAlpha }

function TCustomKeyAlpha.Shift_char(const Value: TKeyAlpha):Char;
begin
  Result := #0;
  if TKeyBoardAlpha.Writable then begin
    Result := Chr( 97 + Integer(Value) - Integer(ka_shift_a) );
    SendKey(VkKeyScan(Result), 10, [ss_lshift])
  end
end;

function TCustomKeyAlpha.Ctrl_char(const Value: TKeyAlpha):Char;
begin
  Result := Chr( 97 + Integer(Value) - Integer(ka_ctrl_a) );
  SendKey(VkKeyScan(Result), 10, [ss_lctrl])
end;

procedure TCustomKeyAlpha.Execute_(const Value: string);
begin
  SetCurrent(Value);
  case FCurrent of
    {Shift+Alpha}
    ka_shift_a..ka_shift_z           : Shift_char          ( FCurrent );
    {Alt+Alpha}
    ka_alt_a..ka_alt_z               : Alt_char            ( FCurrent );
    {Ctrl+Aplha}
    ka_ctrl_a..ka_ctrl_z             : Ctrl_char           ( FCurrent );
    {Alt+Ctrl+Alpha}
    ka_alt_ctrl_a..ka_alt_ctrl_z     : Alt_Ctlr_char       ( FCurrent );
    {Shift+Ctrl+Alpha}
    ka_shift_ctrl_a..ka_shift_ctrl_z : Shift_Ctlr_char     ( FCurrent );
    {Shift+Alt+Alpha}
    ka_shift_alt_a..ka_shift_alt_z   : Shift_Alt_char      ( FCurrent );
    {Shift+Alt+Ctrl+Alpha -- non implémentés vocalement}
    ka_sac_a..ka_sac_z               : Shift_Alt_Ctrl_char ( FCurrent );
    {numeriques}
    ka_0..ka_9                       : SendNumKey          ( FCurrent, [] );
    {Shift+Num}
    ka_shift_0..ka_shift_9           : SendNumKey          ( FCurrent, ka_shift_0,      [ss_lshift] );
    {Alt+Num}
    ka_alt_0..ka_alt_9               : SendNumKey          ( FCurrent, ka_alt_0,        [ss_lalt] );
    {Ctrl+Num}
    ka_ctrl_0..ka_ctrl_9             : SendNumKey          ( FCurrent, ka_ctrl_0,       [ss_lctrl] );
    {Shift+Alt+Num}
    ka_shift_alt_0..ka_shift_alt_9   : SendNumKey          ( FCurrent, ka_shift_alt_0,  [ss_lshift, ss_lalt] );
    {Shift+Ctrl+Num}
    ka_shift_ctrl_0..ka_shift_ctrl_9 : SendNumKey          ( FCurrent, ka_shift_ctrl_0, [ss_lshift, ss_lctrl] );
    {Alt+Ctrl+Num}
    ka_alt_ctrl_0..ka_alt_ctrl_9     : SendNumKey          ( FCurrent, ka_alt_ctrl_0,   [ss_lalt,   ss_lctrl] );
    {Shift+Alt+Ctrl+Num -- vocalement non implémenté}
    ka_sac_0..ka_sac_9               : SendNumKey          ( FCurrent, ka_sac_0,        [ss_lshift, ss_lalt, ss_lctrl] );
  end
end;

procedure TCustomKeyAlpha.SetCurrent(const Value: string);
begin
  FCurrent := StrKeyToAlpha( Value )
end;

function TCustomKeyAlpha.Alt_char(const Value: TKeyAlpha): Char;
begin
  Result := Chr( 97 + Integer(Value) - Integer(ka_alt_a) );
  SendKey(VkKeyScan(Result), 10, [ss_lalt])
end;

function TCustomKeyAlpha.Alt_Ctlr_char(const Value: TKeyAlpha): Char;
begin
  Result := Chr( 97 + Integer(Value) - Integer(ka_alt_ctrl_a) );
  SendKey(VkKeyScan(Result), 10, [ss_lalt, ss_lctrl])
end;

function TCustomKeyAlpha.GetText: string;
begin
  Result := StrArrayToStr( StrKeyAlpha )
end;

function TCustomKeyAlpha.Shift_Ctlr_char(const Value: TKeyAlpha): Char;
begin
  Result := Chr( 97 + Integer(Value) - Integer(ka_shift_ctrl_a) );
  SendKey(VkKeyScan(Result), 10, [ss_lshift, ss_lctrl])
end;

function TCustomKeyAlpha.Shift_Alt_char(const Value: TKeyAlpha): Char;
begin
  Result := Chr( 97 + Integer(Value) - Integer(ka_shift_alt_a) );
  SendKey(VkKeyScan(Result), 10, [ss_lshift, ss_lalt])
end;

function TCustomKeyAlpha.Shift_Alt_Ctrl_char(const Value: TKeyAlpha): Char;
begin
  Result := Chr( 97 + Integer(Value) - Integer(ka_sac_a) );
  SendKey(VkKeyScan(Result), 10, [ss_lshift, ss_lalt, ss_lctrl])
end;

procedure TCustomKeyAlpha.SendNumKey(const AKey: TKeyAlpha; ASpecials: TSpecials);
begin
  case AKey of
    ka_0 : SendKey(VK_NUMPAD0, 10, ASpecials);
    ka_1 : SendKey(VK_NUMPAD1, 10, ASpecials);
    ka_2 : SendKey(VK_NUMPAD2, 10, ASpecials);
    ka_3 : SendKey(VK_NUMPAD3, 10, ASpecials);
    ka_4 : SendKey(VK_NUMPAD4, 10, ASpecials);
    ka_5 : SendKey(VK_NUMPAD5, 10, ASpecials);
    ka_6 : SendKey(VK_NUMPAD6, 10, ASpecials);
    ka_7 : SendKey(VK_NUMPAD7, 10, ASpecials);
    ka_8 : SendKey(VK_NUMPAD8, 10, ASpecials);
    ka_9 : SendKey(VK_NUMPAD9, 10, ASpecials);
  end;
end;

function TCustomKeyAlpha.SendNumKey(const AKey, Ref: TKeyAlpha; ASpecials: TSpecials):TKeyAlpha;
begin
  Result := TKeyAlpha( Integer(ka_0) + Integer(AKey) - Integer(Ref) );
  SendNumKey(AKey, ASpecials)
end;

{ TKeyBoardAlpha }

class procedure TKeyBoardAlpha.Execute(const Value: string);
begin
  if Assigned(KeyBoardAlpha) then with KeyBoardAlpha do Execute_(Value)
end;

class procedure TKeyBoardAlpha.Finalize;
begin
  FreeAndNil( KeyBoardAlpha )
end;

class function TKeyBoardAlpha.Initialize: TKeyBoardAlpha;
begin
  KeyBoardAlpha := TKeyBoardAlpha.Create(Application.MainForm);
  Result        := KeyBoardAlpha
end;

class function TKeyBoardAlpha.Text: string;
begin
  Result := EmptyStr;
  if Assigned(KeyBoardAlpha) then with KeyBoardAlpha do Result := Text_
end;

class function TKeyBoardAlpha.Writable: Boolean;
begin
  Result := TKeyEditors.Writable
end;

{ TCustomKeyDir }

procedure TCustomKeyDir.Execute_(const Value: string);
{WARNING -- kd_sac_X non implémentés vocalement}
var
  ASpecs: TSpecials;

  procedure SpaceProc; begin
    if Value = 'space' then begin
      if IsPenOn then SendKey(VK_SPACE, 10, [])
    end else SendKey(VK_SPACE, 10, ASpecs)
  end;

begin
  SetCurrent(Value);
  case ModuloCode(ASpecs) of
    kd_none                : ;
    kd_left                : SendKey(VK_LEFT,      10, ASpecs);
    kd_right               : SendKey(VK_RIGHT,     10, ASpecs);
    kd_up                  : SendKey(VK_UP,        10, ASpecs);
    kd_down                : SendKey(VK_DOWN,      10, ASpecs);
    kd_home                : SendKey(VK_HOME,      10, ASpecs);
    kd_end                 : SendKey(VK_END,       10, ASpecs);
    kd_esc                 : SendKey(VK_ESCAPE,    10, ASpecs);
    kd_return              : SendKey(VK_RETURN,    10, ASpecs);
    kd_tab                 : SendKey(VK_TAB,       10, ASpecs);
    kd_pagenext            : SendKey(VK_NEXT,      10, ASpecs);
    kd_pageprior           : SendKey(VK_PRIOR,     10, ASpecs);
    kd_space               : SpaceProc;
    kd_backspace           : SendKey(VK_BACK,      10, ASpecs);
    kd_suppr               : SendKey(VK_DELETE,    10, ASpecs);
    kd_insert              : SendKey(VK_INSERT,    10, ASpecs);
    kd_capital             : SendKey(VK_CAPITAL,   10, ASpecs);
    kd_numlock             : SendKey(VK_NUMLOCK,   10, ASpecs);
  end
end; {Execute_}

function TCustomKeyDir.GetText: string;
begin
  Result := StrArrayToStr( StrKeyDir )
end;

function TCustomKeyDir.ModuloCode(var Specs: TSpecials): TKeydir;

  function Translate(const Ref: TKeydir; ASpecs: TSpecials):TKeydir; begin
    Specs  := ASpecs;
    Result := TKeydir(Integer(kd_left) + Integer(FCurrent) - Integer(Ref))
  end;

  function IndexOfKeyDir: Integer; begin
    Result := Integer(Pred(FCurrent)) div Integer(kd_numlock)
  end;

begin
  Result := kd_none;
  Specs := [];
  case IndexOfKeyDir of
    0 : Result := FCurrent;
    1 : Result := Translate(kd_shift_left,  [ss_lshift]);
    2 : Result := Translate(kd_ctrl_left,   [ss_lctrl]);
    3 : Result := Translate(kd_alt_left,    [ss_lalt]);
    4 : Result := Translate(kd_shctrl_left, [ss_lshift, ss_lctrl]);
    5 : Result := Translate(kd_shalt_left,  [ss_lshift, ss_lalt]);
    6 : Result := Translate(kd_alctrl_left, [ss_lalt,   ss_lctrl]);
    7 : Result := Translate(kd_sac_left,    [ss_lshift, ss_lalt, ss_lctrl]);
  end
end; {ModuloCode}

procedure TCustomKeyDir.SetCurrent(const Value: string);
begin
  FCurrent := StrKeyToDir( Value )
end;

{ TKeyBoardDir }

class procedure TKeyBoardDir.Execute(const Value: string);
begin
  if Assigned(KeyBoardDir) then with KeyBoardDir do Execute_(Value)
end;

class procedure TKeyBoardDir.Finalize;
begin
  FreeAndNil( KeyBoardDir )
end;

class function TKeyBoardDir.Initialize: TKeyBoardDir;
begin
  KeyBoardDir := TKeyBoardDir.Create(Application.MainForm);
  Result      := KeyBoardDir
end;

class function TKeyBoardDir.Text: string;
begin
  Result := EmptyStr;
  if Assigned(KeyBoardDir) then with KeyBoardDir do Result := Text_
end;

class function TKeyBoardDir.Writable: Boolean;
begin
  Result := TKeyEditors.Writable
end;

{ TCustomShorcut }

class function TShortcuts.AlphaShortCut: string;
begin
  Result := TKeyBoardAlpha.Text
end;

constructor TShortcuts.Create;
begin
  inherited Create;
  TKeyEditors.Initialize;
  TKeyBoardFx.Initialize;
  TKeyBoardAlpha.Initialize;
  TKeyBoardDir.Initialize;
  TKeyBoardWinApp.Initialize;
  TKeyBoardElite.Initialize;
  TKeyBoardTimer.Initialize;
end;

destructor TShortcuts.Destroy;
begin
  TKeyEditors.Finalize;
  TKeyBoardFx.Finalize;
  TKeyBoardAlpha.Finalize;
  TKeyBoardDir.Finalize;
  TKeyBoardWinApp.Finalize;
  TKeyBoardElite.Finalize;
  TKeyBoardTimer.Finalize;
  inherited;
end;

class function TShortcuts.DirShortCut: string;
begin
  Result := TKeyBoardDir.Text
end;

class function TShortcuts.EditorShortCut: string;
begin
  Result := TKeyEditors.Text
end;

class procedure TShortcuts.Execute(const Value: string);
begin
  Shortcuts.Execute_(Value)
end;

procedure TShortcuts.Execute_(const Value: string);
var
  Ident: TKeyKind;
begin
  Ident := KeyIdentifier(Value);
  case Ident of
    kk_codetouche : ;
    kk_touche     : ;
    kk_function   : TKeyEditors.Execute(Value);
    kk_keyfx      : TKeyBoardFx.Execute(Value);
    kk_keyalpha   : TKeyBoardAlpha.Execute(Value);
    kk_keydir     : TKeyBoardDir.Execute(Value);
    kk_winapp     : TKeyBoardWinApp.Execute(Value);
    kk_elite      : TKeyBoardElite.Execute(Value);
    kk_timer      : TKeyBoardTimer.Execute(Value);
  end;
  case Ident of
    kk_function..kk_timer : CurrentKeyCmdToReg(Value);
  end;
end;

class procedure TShortcuts.Finalize;
begin
  AltRelease_;
  FreeAndNil( Shortcuts )
end;

class function TShortcuts.FxShortCut: string;
begin
  Result := TKeyBoardFx.Text
end;

class function TShortcuts.WinAppShortCut: string;
begin
  Result := TKeyBoardWinApp.Text
end;

class function TShortcuts.EliteShortCut: string;
begin
  Result := TKeyBoardElite.Text
end;

class function TShortcuts.TimerShortCut: string;
begin
  Result := TKeyBoardTimer.Text
end;

function TShortcuts.GetText: string;
begin
  with TStringList.Create do
  try
    Add( TKeyEditors.Text     );
    Add( TKeyBoardFx.Text     );
    Add( TKeyBoardAlpha.Text  );
    Add( TKeyBoardDir.Text    );
    Add( TKeyBoardWinApp.Text );
    Add( TKeyBoardElite.Text  );
    Add( TKeyBoardTimer.Text  );
    Result := Trim(Text);
  finally
    Free
  end
end;

class procedure TShortcuts.Initialize;
begin
  AltRelease_;
  Shortcuts := TShortcuts.Create
end;

class function TShortcuts.Text: string;
begin
  Result := EmptyStr;
  if Assigned(Shortcuts) then with Shortcuts do Result := Text_
end;

{ TCustomKeyWinApp }

procedure TCustomKeyWinApp.Execute_(const Value: string);
begin
  SetCurrent(Value);
  case FCurrent of
    kw_none        : ;
    kx_win         : SendKey(VK_LWIN,   10, []);
    kx_wind        : SendKeyWin( VkKeyScan('d'), 10);
    kx_winm        : SendKeyWin( VkKeyScan('m'), 10);
    kx_wine        : SendKeyWin( VkKeyScan('e'), 10);
    kx_winl        : SendKeyWin( VkKeyScan('l'), 10); //ne peut pas être invoqué via le racourci
    kx_winr        : SendKeyWin( VkKeyScan('r'), 10);
    kx_winp        : SendKeyWin( VkKeyScan('p'), 10);
    kx_winf        : SendKeyWin( VkKeyScan('f'), 10);
    kx_win1        : SendKeyWin( VkKeyScan('1'), 10);
    kx_win2        : SendKeyWin( VkKeyScan('2'), 10);
    kx_win3        : SendKeyWin( VkKeyScan('3'), 10);
    kx_win4        : SendKeyWin( VkKeyScan('4'), 10);
    kx_win5        : SendKeyWin( VkKeyScan('5'), 10);
    kx_win6        : SendKeyWin( VkKeyScan('6'), 10);
    kx_win7        : SendKeyWin( VkKeyScan('7'), 10);
    kx_win8        : SendKeyWin( VkKeyScan('8'), 10);
    kx_win9        : SendKeyWin( VkKeyScan('9'), 10);
    kx_win0        : SendKeyWin( VkKeyScan('0'), 10);
    kx_minimize    : MinimizeActiveWindows;
    kx_restore     : RestoreActiveWindows;
    kx_close       : CloseActiveWindows;
    kx_medplay     : SendKey(VK_MEDIA_PLAY_PAUSE, 10, []);
    kx_medstop     : SendKey(VK_MEDIA_STOP, 10, []);
    kx_mednext     : SendKey(VK_MEDIA_NEXT_TRACK, 10, []);
    kx_medprior    : SendKey(VK_MEDIA_PREV_TRACK, 10, []);
    kx_volmute     : SendKey(VK_VOLUME_MUTE, 10, []);
    kx_voldown     : SendKey(VK_VOLUME_DOWN, 10, []);
    kx_volup       : SendKey(VK_VOLUME_UP, 10, []);
    kx_navback     : SendKey(VK_BROWSER_BACK, 10, []);
    kx_navforward  : SendKey(VK_BROWSER_FORWARD, 10, []);
    kx_navrefresh  : SendKey(VK_BROWSER_REFRESH, 10, []);
    kx_navstop     : SendKey(VK_BROWSER_STOP, 10, []);
    kx_navsearch   : SendKey(VK_BROWSER_SEARCH, 10, []);
    kx_navfavor    : SendKey(VK_BROWSER_FAVORITES, 10, []);
    kx_navhome     : SendKey(VK_BROWSER_HOME, 10, []);
    kx_navgolink   : GoFirstLing;
    kx_navlnext    : SendKey(VK_TAB, 10, []);
    kx_navlprior   : SendKey(VK_TAB, 10, [ss_lshift]);
    kx_navfull     : SendKey(VK_F11, 10, []);
    kx_sheetclose  : SendKey(VkKeyScan('w'), 10, [ss_lctrl]);
    kx_sheetnew    : SendKey(VkKeyScan('t'), 10, [ss_lctrl]);
    kx_sheetnext   : SendKey(VK_NEXT, 10, [ss_lctrl]);
    kx_sheetprior  : SendKey(VK_PRIOR, 10, [ss_lctrl]);
    kx_navnew      : SendKey(VkKeyScan('n'), 10, [ss_lctrl]);
    kx_navzoomin   : SendKey(VK_ADD, 10, [ss_lctrl]);
    kx_navzoomout  : SendKey(VK_SUBTRACT, 10, [ss_lctrl]);
    kx_navnozoom   : SendKey(VkKeyScan('0'), 10, [ss_lctrl]);
    kx_left_clic   : MouseLeftClic;
    kx_right_clic  : MouseRightClic;
    kx_middle_clic : MouseMiddleClic;
    kx_double_clic : MouseDoubleClic;
    kx_notepad     : ExecProcessus('notepad.exe', 'artu.txt');
  end
end;

function TCustomKeyWinApp.GetText: string;
begin
  Result := StrArrayToStr( StrKeyWinApp )
end;

procedure TCustomKeyWinApp.GoFirstLing;
begin
  SendKey(VK_ESCAPE, 10, []);
  SendKey(VkKeyScan('L'), 10, [ss_lctrl]);
  SendKey(VK_RETURN, 10, []);
  Sleep(300);
  SendKey(VK_TAB, 10, []);
  Sleep(300);
  SendKey(VK_RETURN, 10, []);
  Sleep(300);
  SendKey(VK_HOME, 10, [ss_lctrl]);
end;

procedure TCustomKeyWinApp.SetCurrent(const Value: string);
begin
  FCurrent := StrKeyToWinApp( Value )
end;

{ TKeyBoardWinApp }

class procedure TKeyBoardWinApp.Execute(const Value: string);
begin
  if Assigned(KeyBoardWinApp) then with KeyBoardWinApp do Execute_(Value);
end;

class procedure TKeyBoardWinApp.Finalize;
begin
  FreeAndNil( KeyBoardWinApp )
end;

class function TKeyBoardWinApp.Initialize: TKeyBoardWinApp;
begin
  KeyBoardWinApp := TKeyBoardWinApp.Create(Application.MainForm);
  Result         := KeyBoardWinApp
end;

class function TKeyBoardWinApp.Text: string;
begin
  Result := EmptyStr;
  if Assigned(KeyBoardWinApp) then with KeyBoardWinApp do Result := Text_
end;

{ TCustomKeyElite }

procedure TCustomKeyElite.Execute_(const Value: string);
begin
  SetCurrent(Value);
  case FCurrent of
    ke_none   : ;
  end
end;

function TCustomKeyElite.GetText: string;
begin
  Result := StrArrayToStr( StrKeyElite )
end;

procedure TCustomKeyElite.SetCurrent(const Value: string);
begin
  FCurrent := StrKeyToElite( Value )
end;

{ TKeyBoardElite }

class procedure TKeyBoardElite.Execute(const Value: string);
begin
  if Assigned(KeyBoardElite) then with KeyBoardElite do Execute_(Value)
end;

class procedure TKeyBoardElite.Finalize;
begin
  FreeAndNil( KeyBoardElite )
end;

class function TKeyBoardElite.Initialize: TKeyBoardElite;
begin
  KeyBoardElite := TKeyBoardElite.Create(Application.MainForm);
  Result        := KeyBoardElite
end;

class function TKeyBoardElite.Text: string;
begin
  Result := EmptyStr;
  if Assigned(KeyBoardElite) then with KeyBoardElite do Result := Text_
end;

{ TCustomKeyTimer }

procedure TCustomKeyTimer.Execute_(const Value: string);
begin
  SetCurrent( Value );
  case FCurrent of
    kt_none : ;
  end
end;

function TCustomKeyTimer.GetText: string;
begin
  Result := StrArrayToStr( StrKeyTimer )
end;

procedure TCustomKeyTimer.SetCurrent(const Value: string);
begin
  FCurrent := StrKeyToTimer( Value )
end;


{ TKeyBoardTimer }

class procedure TKeyBoardTimer.Execute(const Value: string);
begin
  if Assigned(KeyBoardTimer) then with KeyBoardTimer do Execute_(Value)
end;

class procedure TKeyBoardTimer.Finalize;
begin
  FreeAndNil( KeyBoardTimer )
end;

class function TKeyBoardTimer.Initialize: TKeyBoardTimer;
begin
  KeyBoardTimer := TKeyBoardTimer.Create(Application.MainForm);
  Result        := KeyBoardTimer
end;

class function TKeyBoardTimer.Text: string;
begin
  Result := EmptyStr;
  if Assigned(KeyBoardTimer) then with KeyBoardTimer do Result := Text_
end;

{..............................................................................}

procedure WriteLastCommand(const ASt: string);
begin
  KeyWrite(AppKey, 'LastCommand', ASt);
end;

function ReadLastCommand:string;
begin
  Result := KeyReadString(AppKey, 'LastCommand');
end;

function IsPenOn: Boolean;
begin
  Result := KeyReadBoolean(AppKey, 'pen')
end;

initialization
  StrKeyIdentInitialize;
  StrKeyEditorInitialize;
  StrKeyDirInitialize;
  StrKeyAlphaInitialize;
  StrKeyFxInitialize;
  StrKeyWinAppInitialize;
  StrKeyEliteInitialize;
  StrKeyTimerInitialize;
end.
