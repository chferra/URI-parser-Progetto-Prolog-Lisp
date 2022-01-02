%
%  Chiamate corrette
%
(uri-parse "http://Uinfo@HostX:85/myServer/line/one?QueryXyZ#Fragment-01")	;V
(uri-parse "http://Uinfo@HostX:85/myServer/line/one?QueryXyZ")	;V
(uri-parse "http://Uinfo@HostX:85/myServer/line/one#Fragment-01")	;V 
(uri-parse "http://Uinfo@HostX/myServer/line/one")	;V	
(uri-parse "http://HostX/myServer/line/one")	;V	
(uri-parse "http://HostX.bis/myServer/line/one")	;V	
(uri-parse "http://192.168.1.34/myServer/line/one")	;V	
(uri-parse "http://Uinfo@192.168.1.34/myServer/line/one")	;V 

(uri-parse "http://Uinfo@HostX:85/myServer?QueryXyZ#Fragment-01)	;V
(uri-parse "http://Uinfo@HostX/myServer")	;V
(uri-parse "http://192.168.1.34/myServer")	;V
(uri-parse "http://Uinfo@192.168.1.234")	;V
(uri-parse "http://Uinfo@HostX:85/.?QueryXyZ#Fragment-01")	;V
(uri-parse "http://Uinfo@HostX")	;V
(uri-parse "http://HostX/myServer") 	;V

(uri-parse "http://Uinfo/myServer/line/one?QueryXyZ#Fragment-01")	;V
(uri-parse "http://Uinfo@Host/myServer/line/one?Query#XyZ#Fragment-01")	;V

(uri-parse "mailto:Uinfo@HostX")	;V
(uri-parse "mailto:Uinfo@HostX.yz")	;V
(uri-parse "mailto:Uinfo@201.201.154")	;V
(uri-parse "mailto:Uinfo")	;V

(uri-parse "news:HostX")	;V
(uri-parse "news:HostX.yz")	;V
(uri-parse "news:201.201.154.255")	;V

(uri-parse "tel:Uinfo")	;V
(uri-parse "fax:Uinfo")	;V

(uri-parse "zos://Uinfo@HostX:85/ID44?QueryXyZ#Fragment-01")	;V
(uri-parse "zos://Uinfo@HostX:85/ID44val(id8Val)?QueryXyZ#Fragment-01")	;V
(uri-parse "zos://Uinfo@HostX:85/IDV.ID44val(id8Val)?QueryXyZ#Fragment-01")	;V
(uri-parse "zos://Uinfo@HostX:85/IDV44a.ID44b(id8Val)?QueryXyZ#Fragment-01")	;V
(uri-parse "zos://Uinfo@HostX:85/IDV.ID44val?QueryXyZ#Fragment-01")    ;V
(uri-parse "zos://Uinfo@HostX.yz/ID44?QueryXyZ")	;V
(uri-parse "zos://Uinfo@200.200.201.203/ID44?QueryXyZ#Fragment-01")	;V

%
%  Chiamate che devono generare errore 
%

(uri-parse "http//Uinfo@HostX:85/myServer/line/one?QueryXyZ#Fragment-01")	;V
(uri-parse "http://Uinfo@HostX:85myServer/line/one?QueryXyZ#Fragment-01")	;V
(uri-parse "http:/Uinfo@HostX:85/myServer/line/one?QueryXyZ#Fragment-01")	;V
(uri-parse "http://Uinfo@/myServer/line/one")	;V
(uri-parse "http://Uinfo#a@HostX:85/myServer/line/one?QueryXyZ#Fragment-01")	;V
(uri-parse "http://Uinfo@Host@XXX:85/myServer/line/one?QueryXyZ#Fragment-01")	;V
(uri-parse "http://Uinfo@Host:/myServer/line/one?QueryXyZ#Fragment-01")	;V
(uri-parse "http://Uinfo@HostX.:85/myServer/line/one?QueryXyZ#Fragment-01")	;V
(uri-parse "http://Uinfo@HostX.A:8s/myServer/line/one?QueryXyZ#Fragment-01")	;V
(uri-parse "http://Uinfo@HostX/myServer/line/o@ne?QueryXyZ#Fragment-01")	;V

(uri-parse "news:HostsX/A")	;V
(uri-parse "news:HostsX:A")	;V
(uri-parse "mailtO:Uinfo@HostX")	;XXXXX   Non discrimina la "o" maiuscola  >> forse in Lisp OK
                                    ;Y corretto in uri-parseNew alla riga 249
(uri-parse "mailto:Uinfo@HostX.")	;V
(uri-parse "mailto://Uinfo@HostX/myServer/line/one?QueryXyZ#Fragment-01")	;V
(uri-parse "mailto:Uinfo@HostX/myServer")	;V
(uri-parse "tel:Uinfo@HostX")	;V
(uri-parse "fax:@Uinfo")	;V
(uri-parse "fax:Uin@fo")	;V


(uri-parse "zos://Uinfo@")	;V
(uri-parse "zos://Uinfo@HostX.yz/")	;XXXXX     Error: NIL (of type NULL) is not of type CHARACTER.
(uri-parse "zos://Uinfo@HostX:85/.?QueryXyZ#Fragment-01")	;V
(uri-parse "zos://Uinfo@HostX:85/.(id8Val)#Fragment-01")	;V
(uri-parse "zos://Uinfo@HostX:85/IDV(id8).ID44val(id8Val)?QueryXyZ#Fragment-01")    ;V
(uri-parse "zos://Uinfo@HostX:85/IDV.ID44val.?QueryXyZ#Fragment-01")    ;V
(uri-parse "zos://Uinfo@200.200.201.203/ID441234567890123456789012345678901234567890A?QueryXyZ#Fragment-01")    ;XXXXX    NON vede il superamento dei 44 cratteri per "id44"
(uri-parse "zos://Uinfo@HostX/1IDV.ID44val?QueryXyZ#Fragment-01")    ;V

