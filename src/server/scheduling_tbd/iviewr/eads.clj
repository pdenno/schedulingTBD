(ns scheduling-tbd.iviewr.eads
  "So far, this is required by interviewers, and it only brings in more requirements, the EADSs."
  (:require
   ;; These are all for mount
   [scheduling-tbd.iviewr.domain.data.orm]
   [scheduling-tbd.iviewr.domain.process.flow_shop]
   [scheduling-tbd.iviewr.domain.process.job_shop_c]
   [scheduling-tbd.iviewr.domain.process.job_shop]
   [scheduling-tbd.iviewr.domain.process.job_shop_u]
   [scheduling-tbd.iviewr.domain.process.scheduling-problem-type]
   [scheduling-tbd.iviewr.domain.process.timetabling]))
