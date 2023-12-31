&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          todoer           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE iUserNumCounter AS INTEGER INITIAL 1    NO-UNDO.
DEFINE VARIABLE lPasswordOK AS LOGICAL INITIAL NO    NO-UNDO.
DEFINE VARIABLE cPasswordScreenValue AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES todoer.Users

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME todoer.Users.Username ~
todoer.Users.Password todoer.Users.Nickname todoer.Users.Email 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DEFAULT-FRAME todoer.Users.Username ~
todoer.Users.Password todoer.Users.Nickname todoer.Users.Email 
&Scoped-define ENABLED-TABLES-IN-QUERY-DEFAULT-FRAME todoer.Users
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DEFAULT-FRAME todoer.Users
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH todoer.Users SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH todoer.Users SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME todoer.Users
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME todoer.Users


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS todoer.Users.Username todoer.Users.Password ~
todoer.Users.Nickname todoer.Users.Email 
&Scoped-define ENABLED-TABLES todoer.Users
&Scoped-define FIRST-ENABLED-TABLE todoer.Users
&Scoped-Define ENABLED-OBJECTS fillPasswordCheck BtnDone BtnCancel 
&Scoped-Define DISPLAYED-FIELDS todoer.Users.Username todoer.Users.Password ~
todoer.Users.Nickname todoer.Users.Email 
&Scoped-define DISPLAYED-TABLES todoer.Users
&Scoped-define FIRST-DISPLAYED-TABLE todoer.Users
&Scoped-Define DISPLAYED-OBJECTS fillPasswordCheck 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.12
     BGCOLOR 8 .

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.12
     BGCOLOR 8 .

DEFINE VARIABLE fillPasswordCheck AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 TOOLTIP "Re-enter your password"
     BGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      todoer.Users SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     todoer.Users.Username AT ROW 3 COL 12.57 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 20 BY 1 TOOLTIP "Enter your username"
          BGCOLOR 15 
     todoer.Users.Password AT ROW 5 COL 12.57 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 20 BY 1 TOOLTIP "Enter your password"
          BGCOLOR 15 
     fillPasswordCheck AT ROW 6.5 COL 12.57 COLON-ALIGNED WIDGET-ID 6
     todoer.Users.Nickname AT ROW 8 COL 12.57 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 20 BY 1 TOOLTIP "Enter your nickname"
          BGCOLOR 15 
     todoer.Users.Email AT ROW 9.54 COL 8.14 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 30 BY 1 TOOLTIP "Enter your email"
          BGCOLOR 15 
     BtnDone AT ROW 11.69 COL 16.43 WIDGET-ID 18
     BtnCancel AT ROW 13 COL 16.14 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 44.57 BY 16.5
         BGCOLOR 8 
         DEFAULT-BUTTON BtnDone CANCEL-BUTTON BtnCancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Create account"
         HEIGHT             = 16.5
         WIDTH              = 44.57
         MAX-HEIGHT         = 21.46
         MAX-WIDTH          = 99.14
         VIRTUAL-HEIGHT     = 21.46
         VIRTUAL-WIDTH      = 99.14
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "todoer.Users"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Create account */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Create account */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Done */
DO:
  IF lPasswordOK = YES THEN
  DO:
      RUN CreateUser.
      &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
        &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
          RUN dispatch IN THIS-PROCEDURE ('exit').
        &ELSE
          RUN exitObject.
        &ENDIF
      &ELSE
          APPLY "CLOSE":U TO THIS-PROCEDURE.
      &ENDIF
     END.
  ELSE DO:
      MESSAGE "Your passwords don't match up. Please re-enter your password." VIEW-AS ALERT-BOX.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fillPasswordCheck
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillPasswordCheck C-Win
ON LEAVE OF fillPasswordCheck IN FRAME DEFAULT-FRAME
DO:
  RUN PasswordCheck.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK: 
  RUN enable_UI.
  CLEAR FRAME DEFAULT-FRAME.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateUser C-Win 
PROCEDURE CreateUser :
/*------------------------------------------------------------------------------
  Purpose: Saves the data from the fill in fields to the database.    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   RUN UserNumCounter.
   CREATE Users.
   ASSIGN
       Users.UserNum = iUserNumCounter.
       Users.Username = Users.Username:SCREEN-VALUE.
       Users.Password = Users.Password:SCREEN-VALUE.
       Users.Nickname = Users.Nickname:SCREEN-VALUE.
       Users.Email = Users.Email:SCREEN-VALUE.
       Users.DateCreated = NOW.
   MESSAGE "User" Users.Nickname "created successfully." VIEW-AS ALERT-BOX.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  DISPLAY fillPasswordCheck 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE todoer.Users THEN 
    DISPLAY todoer.Users.Username todoer.Users.Password todoer.Users.Nickname 
          todoer.Users.Email 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE todoer.Users.Username todoer.Users.Password fillPasswordCheck 
         todoer.Users.Nickname todoer.Users.Email BtnDone BtnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PasswordCheck C-Win 
PROCEDURE PasswordCheck :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    IF Users.Password:SCREEN-VALUE = fillPasswordCheck:SCREEN-VALUE THEN
        DO:
            lPasswordOK = YES.
        END.
    ELSE
        lPasswordOK = NO.
        END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UserNumCounter C-Win 
PROCEDURE UserNumCounter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    FIND LAST Users NO-LOCK NO-ERROR.  
    IF AVAILABLE Users THEN
        iUserNumCounter = Users.UserNum + 1.
    ELSE 
        iUserNumCounter = 1.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

