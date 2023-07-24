&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          todoer           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME frameCreateTask
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS frameCreateTask 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER iUserNum AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE iTaskIDCounter AS INTEGER INITIAL 1     NO-UNDO.
DEFINE VARIABLE iCategoryID AS INTEGER     NO-UNDO.
DEFINE VARIABLE cLoggedInUser AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCategories AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frameCreateTask

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES todoer.Tasks

/* Definitions for DIALOG-BOX frameCreateTask                           */
&Scoped-define FIELDS-IN-QUERY-frameCreateTask todoer.Tasks.TaskName ~
todoer.Tasks.DueDate todoer.Tasks.TaskDescription 
&Scoped-define ENABLED-FIELDS-IN-QUERY-frameCreateTask ~
todoer.Tasks.TaskName todoer.Tasks.DueDate todoer.Tasks.TaskDescription 
&Scoped-define ENABLED-TABLES-IN-QUERY-frameCreateTask todoer.Tasks
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-frameCreateTask todoer.Tasks
&Scoped-define QUERY-STRING-frameCreateTask FOR EACH todoer.Tasks SHARE-LOCK
&Scoped-define OPEN-QUERY-frameCreateTask OPEN QUERY frameCreateTask FOR EACH todoer.Tasks SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-frameCreateTask todoer.Tasks
&Scoped-define FIRST-TABLE-IN-QUERY-frameCreateTask todoer.Tasks


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS todoer.Tasks.TaskName todoer.Tasks.DueDate ~
todoer.Tasks.TaskDescription 
&Scoped-define ENABLED-TABLES todoer.Tasks
&Scoped-define FIRST-ENABLED-TABLE todoer.Tasks
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-FIELDS todoer.Tasks.TaskName todoer.Tasks.DueDate ~
todoer.Tasks.TaskDescription 
&Scoped-define DISPLAYED-TABLES todoer.Tasks
&Scoped-define FIRST-DISPLAYED-TABLE todoer.Tasks


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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY frameCreateTask FOR 
      todoer.Tasks SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frameCreateTask
     todoer.Tasks.TaskName AT ROW 4.08 COL 25 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 
     todoer.Tasks.DueDate AT ROW 5.96 COL 27.43 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     todoer.Tasks.TaskDescription AT ROW 10.31 COL 15.14 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 48.72 BY 7.12
          BGCOLOR 15 
     Btn_OK AT ROW 19.23 COL 28.72
     Btn_Cancel AT ROW 20.96 COL 28.29
     SPACE(34.99) SKIP(0.69)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Create new task"
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
/* SETTINGS FOR DIALOG-BOX frameCreateTask
   FRAME-NAME                                                           */
ASSIGN 
       FRAME frameCreateTask:SCROLLABLE       = FALSE
       FRAME frameCreateTask:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX frameCreateTask
/* Query rebuild information for DIALOG-BOX frameCreateTask
     _TblList          = "todoer.Tasks"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX frameCreateTask */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME frameCreateTask
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frameCreateTask frameCreateTask
ON WINDOW-CLOSE OF FRAME frameCreateTask /* Create new task */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK frameCreateTask
ON CHOOSE OF Btn_OK IN FRAME frameCreateTask /* OK */
DO:
  RUN SaveTask.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK frameCreateTask 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
 
  RUN enable_UI.
  CLEAR FRAME {&FRAME-NAME}.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI frameCreateTask  _DEFAULT-DISABLE
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
  HIDE FRAME frameCreateTask.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI frameCreateTask  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-frameCreateTask}
  GET FIRST frameCreateTask.
  IF AVAILABLE todoer.Tasks THEN 
    DISPLAY todoer.Tasks.TaskName todoer.Tasks.DueDate 
          todoer.Tasks.TaskDescription 
      WITH FRAME frameCreateTask.
  ENABLE todoer.Tasks.TaskName todoer.Tasks.DueDate 
         todoer.Tasks.TaskDescription Btn_OK Btn_Cancel 
      WITH FRAME frameCreateTask.
  VIEW FRAME frameCreateTask.
  {&OPEN-BROWSERS-IN-QUERY-frameCreateTask}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FindCategoryID frameCreateTask 
PROCEDURE FindCategoryID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* DO WITH FRAME {&FRAME-NAME}:                                                                                     */
/*     FIND FIRST Categories WHERE Categories.CategoryName = Categories.CategoryName:SCREEN-VALUE NO-LOCK NO-ERROR. */
/*     IF AVAILABLE Categories THEN                                                                                 */
/*          iCategoryID = Categories.CategoryID.                                                                    */
/*     END.                                                                                                         */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoggedInUser frameCreateTask 
PROCEDURE LoggedInUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveTask frameCreateTask 
PROCEDURE SaveTask :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   RUN TaskIDCounter.
   CREATE Tasks.
   ASSIGN
       Tasks.UserNum = iUserNum.
       Tasks.TaskID = iTaskIDCounter.
       Tasks.DueDate = DATE(Tasks.DueDate:SCREEN-VALUE).
/*        Tasks.CategoryID = iCategoryID. */
       Tasks.TaskDescription = Tasks.TaskDescription:SCREEN-VALUE.
       Tasks.TaskName = Tasks.TaskName:SCREEN-VALUE.
       Tasks.DateCreated = NOW.
       Tasks.CompletionStatus = NO.
   MESSAGE "Task" Tasks.TaskName "created successfully." VIEW-AS ALERT-BOX.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TaskIDCounter frameCreateTask 
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

