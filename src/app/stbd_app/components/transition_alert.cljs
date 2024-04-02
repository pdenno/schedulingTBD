(ns stbd-app.components.transition-alert
  (:require
   [helix.core :refer [defnc $]]
   [helix.hooks :as hooks]
   ["@mui/icons-material/Close$default" :as CloseIcon]
   ["@mui/material/Alert$default" :as Alert]
   ["@mui/material/Dialog$default" :as Dialog]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Button$default" :as Button]
   ["@mui/material/Collapse$default" :as Collapse]
   ["@mui/material/IconButton$default" :as IconButton]))

(defnc TransitionAlert []
  (let [[open, set-open] (hooks/use-state false)]
    ($ Box {:width "60%"}
       ($ Collapse {:in open}
          ($ Alert
                "Close me!"
                ($ IconButton {:aria-label "close"
                               :color "inherit"
                               :size "small"
                               :onClick (fn [& _] (set-open false))}
                   ($ CloseIcon {:fontSize "inherit"}))))
       ($ Button {:disabled open
                  :variant "outlined"
                  :onClick (fn [& _] (set-open true))}
          "Re-open"))))


;;; import * as React from 'react';
;;; import Box from '@mui/material/Box';
;;; import Alert from '@mui/material/Alert';
;;; import IconButton from '@mui/material/IconButton';
;;; import Collapse from '@mui/material/Collapse';
;;; import Button from '@mui/material/Button';
;;; import CloseIcon from '@mui/icons-material/Close';
;;;
;;; export default function TransitionAlerts() {
;;;   const [open, setOpen] = React.useState(true);
;;;
;;;   return (
;;;     <Box sx={{ width: '100%' }}>
;;;       <Collapse in={open}>
;;;         <Alert
;;;           action={
;;;             <IconButton
;;;               aria-label="close"
;;;               color="inherit"
;;;               size="small"
;;;               onClick={() => {
;;;                 setOpen(false);
;;;               }}
;;;             >
;;;               <CloseIcon fontSize="inherit" />
;;;             </IconButton>
;;;           }
;;;           sx={{ mb: 2 }}
;;;         >
;;;           Close me!
;;;         </Alert>
;;;       </Collapse>
;;;       <Button
;;;         disabled={open}
;;;         variant="outlined"
;;;         onClick={() => {
;;;           setOpen(true);
;;;         }}
;;;       >
;;;         Re-open
;;;       </Button>
;;;     </Box>
;;;   );
;;; }
