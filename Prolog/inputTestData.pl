%
%  Chiamate corrette
%
uri_parse("http://Uinfo@HostX:85/myServer/line/one?QueryXyZ#Fragment-01", URI).
uri_parse("http://Uinfo@HostX:85/myServer/line/one?QueryXyZ", URI).
uri_parse("http://Uinfo@HostX:85/myServer/line/one#Fragment-01", URI).
uri_parse("http://Uinfo@HostX/myServer/line/one", URI).
uri_parse("http://HostX/myServer/line/one", URI).
uri_parse("http://HostX.bis/myServer/line/one", URI).
uri_parse("http://192.168.1.34/myServer/line/one", URI).
uri_parse("http://Uinfo@192.168.1.34/myServer/line/one", URI).
uri_parse("http://Uinfo@192.168.1.234", URI).
uri_parse("http://Uinfo@HostX", URI).

uri_parse("mailto:Uinfo@HostX", URI).
uri_parse("mailto:Uinfo@HostX.yz", URI).
uri_parse("mailto:Uinfo@201.201.154", URI).
uri_parse("mailto:Uinfo", URI).

uri_parse("news:HostX", URI).
uri_parse("news:HostX.yz", URI).
uri_parse("news:201.201.154.255", URI).

uri_parse("tel:Uinfo", URI).
uri_parse("fax:Uinfo", URI).

uri_parse("zos://Uinfo@HostX:85/ID44?QueryXyZ#Fragment-01", URI).
uri_parse("zos://Uinfo@HostX:85/ID44val(id8Val)?QueryXyZ#Fragment-01", URI).
uri_parse("zos://Uinfo@HostX:85/IDV.ID44val(id8Val)?QueryXyZ#Fragment-01", URI).
uri_parse("zos://Uinfo@HostX:85/IDV(id8).ID44val(id8Val)?QueryXyZ#Fragment-01", URI).   % Da' errore ma non dovrebbe
uri_parse("zos://Uinfo@HostX:85/.?QueryXyZ#Fragment-01", URI).
uri_parse("zos://Uinfo@HostX:85/.(id8Val)#Fragment-01", URI).
uri_parse("zos://Uinfo@HostX.yz/ID44?QueryXyZ", URI).
uri_parse("zos://Uinfo@200.200.201.203/ID44?QueryXyZ#Fragment-01", URI).

%
%  Chiamate che devono generare errore 
%
uri_parse("http://a/myServer/line/one?QueryXyZ#Fragment-01", URI).
uri_parse("http://Uinfo@/myServer/line/one", URI).
uri_parse("http://Uinfo@192.168.1.334", URI).    % NON da' errore !!!
uri_parse("http://Uinfo@192.168.334", URI).      % NON da' errore !!!

uri_parse("mailto:Uinfo@201.201.354", URI).   	 % NON da' errore !!!
uri_parse("zos://Uinfo@200.200.201.263/ID44?QueryXyZ#Fragment-01", URI).		% NON da' errore !!!

uri_parse("zos://Uinfo@", URI).
uri_parse("zos://Uinfo@HostX.yz/", URI).

%
% Le chiamate con riferimento a IP-ADDRESS non sono del tutto corrette
% nel senso che se per errore non ci sono 4 gruppi o le parti numeriche 
% sono fuori range (0..255), si ha ancora esito positivo trattando IP come un hostname
% Ho provto qualche alternativa ma non sono riuscito a fixare il problema
%