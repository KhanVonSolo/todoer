&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          todoer           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME CreateTaskDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS CreateTaskDialog 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      iUserNum

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER iUserNum AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE iTaskIDCounter AS INTEGER INITIAL 1     NO-UNDO.
DEFINE VARIABLE cLoggedInUser AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCategories AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME CreateTaskDialog

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES todoer.Tasks todoer.Categories

/* Definitions for DIALOG-BOX CreateTaskDialog                          */
&Scoped-define FIELDS-IN-QUERY-CreateTaskDialog todoer.Tasks.TaskName ~
todoer.Tasks.DueDate todoer.Tasks.TaskDescription 
&Scoped-define ENABLED-FIELDS-IN-QUERY-CreateTaskDialog ~
todoer.Tasks.TaskName todoer.Tasks.DueDate todoer.Tasks.TaskDescription 
&Scoped-define ENABLED-TABLES-IN-QUERY-CreateTaskDialog todoer.Tasks
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-CreateTaskDialog todoer.Tasks
&Scoped-define QUERY-STRING-CreateTaskDialog FOR EACH todoer.Tasks SHARE-LOCK, ~
      EACH todoer.Categories WHERE Categories.CategoryID ~
  AND Categories.CategoryName SHARE-LOCK
&Scoped-define OPEN-QUERY-CreateTaskDialog OPEN QUERY CreateTaskDialog FOR EACH todoer.Tasks SHARE-LOCK, ~
      EACH todoer.Categories WHERE Categories.CategoryID ~
  AND Categories.CategoryName SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-CreateTaskDialog todoer.Tasks ~
todoer.Categories
&Scoped-define FIRST-TABLE-IN-QUERY-CreateTaskDialog todoer.Tasks
&Scoped-define SECOND-TABLE-IN-QUERY-CreateTaskDialog todoer.Categories


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS todoer.Tasks.TaskName todoer.Tasks.DueDate ~
todoer.Tasks.TaskDescription 
&Scoped-define ENABLED-TABLES todoer.Tasks
&Scoped-define FIRST-ENABLED-TABLE todoer.Tasks
&Scoped-Define ENABLED-OBJECTS cbCategories Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-FIELDS todoer.Tasks.TaskName todoer.Tasks.DueDate ~
todoer.Tasks.TaskDescription 
&Scoped-define DISPLAYED-TABLES todoer.Tasks
&Scoped-define FIRST-DISPLAYED-TABLE todoer.Tasks
&Scoped-Define DISPLAYED-OBJECTS cbCategories 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

DEFINE VARIABLE cbCategories AS CHARACTER FORMAT "x(20)" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY .92.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY CreateTaskDialog FOR 
      todoer.Tasks, 
      todoer.Categories SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME CreateTaskDialog
     todoer.Tasks.TaskName AT ROW 2.85 COL 19.29 COLON-ALIGNED NO-LABEL WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
     todoer.Tasks.DueDate AT ROW 4.31 COL 23.57 COLON-ALIGNED NO-LABEL WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     cbCategories AT ROW 5.96 COL 22.29 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     todoer.Tasks.TaskDescription AT ROW 8.54 COL 7.72 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 50 BY 6.69
     Btn_OK AT ROW 15.96 COL 24.29
     Btn_Cancel AT ROW 17.46 COL 24.57
     "Category:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 6.08 COL 12.57 WIDGET-ID 12
     "Due Date:" VIEW-AS TEXT
          SIZE 9.57 BY .62 AT ROW 4.77 COL 13.43 WIDGET-ID 14
     "Task Name:" VIEW-AS TEXT
          SIZE 11.43 BY .62 AT ROW 3.08 COL 8.86 WIDGET-ID 16
     SPACE(46.84) SKIP(15.33)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Create a new task"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX CreateTaskDialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME CreateTaskDialog:SCROLLABLE       = FALSE
       FRAME CreateTaskDialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX CreateTaskDialog
/* Query rebuild information for DIALOG-BOX CreateTaskDialog
     _TblList          = "todoer.Tasks,todoer.Categories WHERE todoer.Tasks ..."
     _Options          = "SHARE-LOCK"
     _JoinCode[2]      = "Categories.CategoryID
  AND Categories.CategoryName"
     _Query            is OPENED
*/  /* DIALOG-BOX CreateTaskDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME CreateTaskDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CreateTaskDialog CreateTaskDialog
ON WINDOW-CLOSE OF FRAME CreateTaskDialog /* Create a new task */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK CreateTaskDialog 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

/*   DEFINE TEMP-TABLE ttCategories NO-UNDO */
/*     FIELD CategoryID AS INTEGER          */
/*     FIELD CategoryName AS CHARACTER.     */
/*   FOR EACH Categories NO-LOCK:                                 */
/*       CREATE ttCategories.                                     */
/*       ASSIGN                                                   */
/*           ttCategories.CategoryID = Categories.CategoryID      */
/*           ttCategories.CategoryName = Categories.CategoryName. */
/*       cbCategories:ADD-ITEM                                    */
/*   END.                                                         */

/*   RUN FillCategories. */
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI CreateTaskDialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME CreateTaskDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI CreateTaskDialog  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-CreateTaskDialog}
  GET FIRST CreateTaskDialog.
  DISPLAY cbCategories 
      WITH FRAME CreateTaskDialog.
  IF AVAILABLE todoer.Tasks THEN 
    DISPLAY todoer.Tasks.TaskName todoer.Tasks.DueDate 
          todoer.Tasks.TaskDescription 
      WITH FRAME CreateTaskDialog.
  ENABLE todoer.Tasks.TaskName todoer.Tasks.DueDate cbCategories 
         todoer.Tasks.TaskDescription Btn_OK Btn_Cancel 
      WITH FRAME CreateTaskDialog.
  VIEW FRAME CreateTaskDialog.
  {&OPEN-BROWSERS-IN-QUERY-CreateTaskDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillCategories CreateTaskDialog 
PROCEDURE FillCategories :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* DO WITH FRAME {&FRAME-NAME}:                                              */
/*                                                                           */
/*     FOR EACH Categories NO-LOCK:                                          */
/*         ASSIGN cCategories = cCategories + Categories.CategoryName + ",". */
/*     END.                                                                  */
/*                                                                           */
/*     cCategories = cCategories + "Undefined".                              */
/*                                                                           */
/*     ASSIGN cbCategories:LIST-ITEMS IN FRAME {&FRAME-NAME} = cCategories.  */
/*                                                                           */
/* END.                                                                      */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoggedInUser CreateTaskDialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveTask CreateTaskDialog 
PROCEDURE SaveTask :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   RUN UserNumCounter.
   CREATE Tasks.
   ASSIGN
       Tasks.UserNum = iUserNum.
       Tasks.TaskID = iTaskIDCounter.
       Tasks.DueDate = DATE(Tasks.DueDate:SCREEN-VALUE).
       Categories.CategoryName = cbCategories:SCREEN-VALUE.
       Tasks.TaskDescription = Tasks.TaskDescription:SCREEN-VALUE.
       Tasks.TaskName = Tasks.TaskName:SCREEN-VALUE.
       Tasks.DateCreated = NOW.
   MESSAGE "Task" Tasks.TaskName "created successfully." VIEW-AS ALERT-BOX.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TaskIDCounter CreateTaskDialog 
PROCEDURE TaskIDCounter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    FIND LAST Tasks NO-LOCK NO-ERROR.  
    IF AVAILABLE Tasks THEN
        iTaskIDCounter = Tasks.TaskID + 1.
    ELSE 
        iTaskIDCounter = 1.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

