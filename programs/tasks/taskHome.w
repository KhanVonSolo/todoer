&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          todoer           PROGRESS
*/
&Scoped-define WINDOW-NAME todoerHome
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS todoerHome 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      Users.UserNum - inputs the Userum of the currently logged in user.

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

DEFINE INPUT PARAMETER iUserNum AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cLoggedInUser AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lConfirm AS LOGICAL INITIAL NO     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME browseTasks

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES todoer.Tasks

/* Definitions for BROWSE browseTasks                                   */
&Scoped-define FIELDS-IN-QUERY-browseTasks todoer.Tasks.TaskName ~
todoer.Tasks.DueDate todoer.Tasks.CompletionStatus todoer.Tasks.DateCreated ~
todoer.Tasks.CategoryID todoer.Tasks.TaskID 
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseTasks 
&Scoped-define QUERY-STRING-browseTasks FOR EACH todoer.Tasks NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-browseTasks OPEN QUERY browseTasks FOR EACH todoer.Tasks NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-browseTasks todoer.Tasks
&Scoped-define FIRST-TABLE-IN-QUERY-browseTasks todoer.Tasks


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-browseTasks}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 browseTasks btnRefresh ~
btnCreate btnDelete btnEdit btnMark btnDetails 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR todoerHome AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCreate 
     LABEL "Create New Task" 
     SIZE 20 BY 1.12.

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "img/remove.ico":U
     LABEL "Delete" 
     SIZE 8.14 BY 1.96.

DEFINE BUTTON btnDetails 
     LABEL "View Task Details" 
     SIZE 20 BY 1.12.

DEFINE BUTTON btnEdit 
     LABEL "Edit Task" 
     SIZE 20 BY 1.12.

DEFINE BUTTON btnMark 
     LABEL "Mark as Complete" 
     SIZE 20 BY 1.12.

DEFINE BUTTON btnRefresh 
     IMAGE-UP FILE "img/refresh.ico":U
     LABEL "Refresh" 
     SIZE 8.86 BY 2.12.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27.57 BY 9.08.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75.86 BY 9.42.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66.43 BY 5.15
     BGCOLOR 0 FGCOLOR 0 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseTasks FOR 
      todoer.Tasks SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseTasks
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseTasks todoerHome _STRUCTURED
  QUERY browseTasks NO-LOCK DISPLAY
      todoer.Tasks.TaskName COLUMN-LABEL "Name" FORMAT "x(30)":U
            WIDTH 17.14
      todoer.Tasks.DueDate COLUMN-LABEL "Due" FORMAT "99/99/9999":U
            WIDTH 12.86
      todoer.Tasks.CompletionStatus COLUMN-LABEL "Done?" FORMAT "yes/no":U
            WIDTH 14.43
      todoer.Tasks.DateCreated COLUMN-LABEL "Created" FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 17.29
      todoer.Tasks.CategoryID FORMAT ">>>>>9":U
      todoer.Tasks.TaskID FORMAT ">>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 66.43 BY 7.04
         BGCOLOR 15  FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     browseTasks AT ROW 2.42 COL 7 WIDGET-ID 200
     btnRefresh AT ROW 18.5 COL 9.72 WIDGET-ID 28
     btnCreate AT ROW 20.96 COL 29.43 WIDGET-ID 10
     btnDelete AT ROW 21.38 COL 10.14 WIDGET-ID 30
     btnEdit AT ROW 22.38 COL 29.43 WIDGET-ID 12
     btnMark AT ROW 23.77 COL 29 WIDGET-ID 14
     btnDetails AT ROW 25.31 COL 29.43 WIDGET-ID 16
     "Sorting and Filtering Options" VIEW-AS TEXT
          SIZE 29.29 BY .62 AT ROW 11.27 COL 25.57 WIDGET-ID 26
     "Task Summary" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 1.58 COL 31.86 WIDGET-ID 8
     "Actions" VIEW-AS TEXT
          SIZE 9.86 BY .62 AT ROW 18.65 COL 33.43 WIDGET-ID 18
     RECT-1 AT ROW 18.15 COL 25.29 WIDGET-ID 20
     RECT-2 AT ROW 1.31 COL 1.86 WIDGET-ID 22
     RECT-3 AT ROW 11 COL 7.14 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 77.43 BY 28.69
         BGCOLOR 8  WIDGET-ID 100.


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
  CREATE WINDOW todoerHome ASSIGN
         HIDDEN             = YES
         TITLE              = "Todoer - Home"
         HEIGHT             = 28.69
         WIDTH              = 77.43
         MAX-HEIGHT         = 38.81
         MAX-WIDTH          = 274.29
         VIRTUAL-HEIGHT     = 38.81
         VIRTUAL-WIDTH      = 274.29
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
/* SETTINGS FOR WINDOW todoerHome
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB browseTasks RECT-3 DEFAULT-FRAME */
ASSIGN 
       Tasks.CategoryID:VISIBLE IN BROWSE browseTasks = FALSE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(todoerHome)
THEN todoerHome:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseTasks
/* Query rebuild information for BROWSE browseTasks
     _TblList          = "todoer.Tasks"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > todoer.Tasks.TaskName
"TaskName" "Name" ? "character" ? ? ? ? ? ? no ? no no "17.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > todoer.Tasks.DueDate
"DueDate" "Due" ? "date" ? ? ? ? ? ? no ? no no "12.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > todoer.Tasks.CompletionStatus
"CompletionStatus" "Done?" ? "logical" ? ? ? ? ? ? no ? no no "14.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > todoer.Tasks.DateCreated
"DateCreated" "Created" ? "datetime" ? ? ? ? ? ? no ? no no "17.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > todoer.Tasks.CategoryID
"CategoryID" ? ? "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = todoer.Tasks.TaskID
     _Query            is OPENED
*/  /* BROWSE browseTasks */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME todoerHome
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL todoerHome todoerHome
ON END-ERROR OF todoerHome /* Todoer - Home */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL todoerHome todoerHome
ON WINDOW-CLOSE OF todoerHome /* Todoer - Home */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCreate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCreate todoerHome
ON CHOOSE OF btnCreate IN FRAME DEFAULT-FRAME /* Create New Task */
DO:
  RUN taskCreate.w (INPUT iUserNum).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete todoerHome
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Delete */
DO:
  RUN taskDelete.w (INPUT Tasks.TaskID).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDetails todoerHome
ON CHOOSE OF btnDetails IN FRAME DEFAULT-FRAME /* View Task Details */
DO:
  RUN taskDetails.w (INPUT Tasks.TaskID).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEdit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEdit todoerHome
ON CHOOSE OF btnEdit IN FRAME DEFAULT-FRAME /* Edit Task */
DO:
  RUN taskEdit.w (INPUT Tasks.TaskID).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMark
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMark todoerHome
ON CHOOSE OF btnMark IN FRAME DEFAULT-FRAME /* Mark as Complete */
DO:
  MESSAGE Tasks.TaskID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRefresh todoerHome
ON CHOOSE OF btnRefresh IN FRAME DEFAULT-FRAME /* Refresh */
DO:
      browse browseTasks:refresh() NO-ERROR.

 {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseTasks
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK todoerHome 


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
  RUN LoggedInUser.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dfsw todoerHome 
PROCEDURE dfsw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* DO WITH FRAME {&FRAME-NAME}: */
/*     DEFINE INPUT PARAMETER iTaskID AS INTEGER NO-UNDO.                                                                                   */

/*     DEFINE VARIABLE cTaskRow AS ROWID     NO-UNDO. */
/*     cTaskRow = ROWID(Tasks).                       */
/*     END.                                           */
    
/*     FIND FIRST Tasks WHERE Tasks.ROWID = cTaskRow.                                                                                              */
/*     IF AVAILABLE Tasks THEN DO:                                                                                                                 */
/*         MESSAGE "Are you sure you want to delete task" Tasks.TaskName "?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lConfirm AS LOGICAL. */
/*         IF lConfirm then                                                                                                                        */
/*             DELETE Tasks.                                                                                                                       */
/*         ELSE                                                                                                                                    */
/*             MESSAGE "Select a task to delete." VIEW-AS ALERT-BOX.                                                                               */
/*         END.                                                                                                                                    */
    
/* FIND FIRST Tasks WHERE Tasks.TaskID = iTaskID.                                                                                             */
/* IF AVAILABLE Tasks THEN DO:                                                                                                                */
/*     MESSAGE "Are you sure you want to delete task" Tasks.Taskname "?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lConfir AS LOGICAL. */
/*     IF lConfirm THEN                                                                                                                       */
/*         DELETE Tasks.                                                                                                                      */
/*     ELSE                                                                                                                                   */
/*         MESSAGE "Select a task to delete." VIEW-AS ALERT-BOX.                                                                              */
/*     END.                                                                                                                                   */
/* END. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI todoerHome  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(todoerHome)
  THEN DELETE WIDGET todoerHome.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayTasksBrowse todoerHome 
PROCEDURE DisplayTasksBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI todoerHome  _DEFAULT-ENABLE
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
  ENABLE RECT-1 RECT-2 RECT-3 browseTasks btnRefresh btnCreate btnDelete 
         btnEdit btnMark btnDetails 
      WITH FRAME DEFAULT-FRAME IN WINDOW todoerHome.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW todoerHome.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoggedInUser todoerHome 
PROCEDURE LoggedInUser :
/*------------------------------------------------------------------------------
  Purpose: This procedure associates the currently logged in users UserNum
    passed through the login window via iUserNum with the appropriate nickname.     
  Parameters:  iUserNum - to be used as Users.UserNum.
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST Users WHERE Users.UserNum = iUserNum NO-LOCK NO-ERROR.
    IF AVAILABLE Users THEN
        cLoggedInUser = Users.Nickname.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulateTasksBuffer todoerHome 
PROCEDURE PopulateTasksBuffer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* DELETE ttTasks.                                            */
/* FOR EACH Tasks:                                            */
/*     CREATE ttTasks.                                        */
/*     ASSIGN                                                 */
/*         ttTasks.TaskName = Tasks.TaskName                  */
/*         ttTasks.DueDate = Tasks.DueDate                    */
/*         ttTasks.CompletionStatus = Tasks.CompletionStatus. */
/*     END.                                                   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

