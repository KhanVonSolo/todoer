&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          todoer           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      iTaskID - serves as an identifier for the selected task.

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER iTaskID AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE lConfirm AS LOGICAL INITIAL NO     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES todoer.Tasks

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame todoer.Tasks.TaskID ~
todoer.Tasks.TaskName todoer.Tasks.DueDate todoer.Tasks.DateCreated ~
todoer.Tasks.CompletionStatus todoer.Tasks.TaskDescription 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame todoer.Tasks.TaskName ~
todoer.Tasks.DueDate todoer.Tasks.CompletionStatus ~
todoer.Tasks.TaskDescription 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame todoer.Tasks
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame todoer.Tasks
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH todoer.Tasks SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH todoer.Tasks SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame todoer.Tasks
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame todoer.Tasks


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS todoer.Tasks.TaskName todoer.Tasks.DueDate ~
todoer.Tasks.CompletionStatus todoer.Tasks.TaskDescription 
&Scoped-define ENABLED-TABLES todoer.Tasks
&Scoped-define FIRST-ENABLED-TABLE todoer.Tasks
&Scoped-Define ENABLED-OBJECTS Btn_Cancel btnOk 
&Scoped-Define DISPLAYED-FIELDS todoer.Tasks.TaskID todoer.Tasks.TaskName ~
todoer.Tasks.DueDate todoer.Tasks.DateCreated todoer.Tasks.CompletionStatus ~
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
DEFINE BUTTON btnOk AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      todoer.Tasks SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     todoer.Tasks.TaskID AT ROW 3.23 COL 19 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 8.29 BY 1
          BGCOLOR 15 
     todoer.Tasks.TaskName AT ROW 4.23 COL 19 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 31.43 BY 1
          BGCOLOR 15 
     todoer.Tasks.DueDate AT ROW 6.23 COL 19 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 12.86 BY 1
          BGCOLOR 15 
     todoer.Tasks.DateCreated AT ROW 7.23 COL 19 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 27.72 BY 1
          BGCOLOR 15 
     todoer.Tasks.CompletionStatus AT ROW 9.42 COL 22.72 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "YES","NO" 
          DROP-DOWN-LIST
          SIZE 16 BY .88
          BGCOLOR 15 
     todoer.Tasks.TaskDescription AT ROW 11.42 COL 14 NO-LABEL WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 40.57 BY 7.27
          BGCOLOR 15 
     Btn_Cancel AT ROW 21.88 COL 26.43
     btnOk AT ROW 23.42 COL 26.43
     SPACE(31.42) SKIP(1.30)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "<insert dialog title>"
         DEFAULT-BUTTON btnOk CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN todoer.Tasks.DateCreated IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN todoer.Tasks.TaskDescription IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN todoer.Tasks.TaskID IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "todoer.Tasks"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk Dialog-Frame
ON CHOOSE OF btnOk IN FRAME Dialog-Frame /* OK */
DO:
  RUN saveChanges.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


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
  RUN fillFields.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  IF AVAILABLE todoer.Tasks THEN 
    DISPLAY todoer.Tasks.TaskID todoer.Tasks.TaskName todoer.Tasks.DueDate 
          todoer.Tasks.DateCreated todoer.Tasks.CompletionStatus 
          todoer.Tasks.TaskDescription 
      WITH FRAME Dialog-Frame.
  ENABLE todoer.Tasks.TaskName todoer.Tasks.DueDate 
         todoer.Tasks.CompletionStatus todoer.Tasks.TaskDescription Btn_Cancel 
         btnOk 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillFields Dialog-Frame 
PROCEDURE FillFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    FIND Tasks WHERE Tasks.TaskID = iTaskID NO-ERROR.
    IF AVAILABLE Task THEN DO:
        DISPLAY {&Fields-in-query-{&FRAME-NAME}} WITH FRAME {&FRAME-NAME}.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveChanges Dialog-Frame 
PROCEDURE SaveChanges :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    MESSAGE "Do you want to edit task" Tasks.TaskName "?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lConfirm AS LOGICAL.
    IF lConfirm THEN

        ASSIGN
            Tasks.CompletionStatus = LOGICAL(Tasks.CompletionStatus:SCREEN-VALUE).
            Tasks.DueDate = DATE(Tasks.DueDate).
            Tasks.TaskDescription = Tasks.TaskDescription:SCREEN-VALUE.
            Tasks.TaskName = Tasks.TaskName:SCREEN-VALUE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

