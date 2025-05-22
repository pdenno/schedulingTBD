(ns scheduling-tbd.interviewing.eads
  "So far, this is required by interviewers, and it only brings in more reqiurements, the EADSs."
  (:require
   ;; These are all for mount
   [scheduling-tbd.interviewing.domain.data.orm]
   [scheduling-tbd.interviewing.domain.process.flow_shop]
   [scheduling-tbd.interviewing.domain.process.job_shop_c]
   [scheduling-tbd.interviewing.domain.process.job_shop]
   [scheduling-tbd.interviewing.domain.process.job_shop_u]
   [scheduling-tbd.interviewing.domain.process.timetabling]))
