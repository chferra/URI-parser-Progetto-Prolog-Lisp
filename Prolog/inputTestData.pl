%
%  Chiamate corrette
%
uri_parse("http://Uinfo@HostX:85/myServer/line/one?QueryXyZ#Fragment-01", URI).	%V
uri_parse("http://Uinfo@HostX:85/myServer/line/one?QueryXyZ", URI).	%V
uri_parse("http://Uinfo@HostX:85/myServer/line/one#Fragment-01", URI).	%V
uri_parse("http://Uinfo@HostX/myServer/line/one", URI).	%V
uri_parse("http://HostX/myServer/line/one", URI).	%V
uri_parse("http://HostX.bis/myServer/line/one", URI).	%V
uri_parse("http://192.168.1.34/myServer/line/one", URI).	%V
uri_parse("http://Uinfo@192.168.1.34/myServer/line/one", URI).	%V
uri_parse("http://Uinfo@192.168.1.234", URI).	%V
uri_parse("http://Uinfo@HostX:85/.?QueryXyZ#Fragment-01", URI).	%V
uri_parse("http://Uinfo@HostX", URI).	%V
uri_parse("http://Uinfo/myServer/line/one?QueryXyZ#Fragment-01", URI).	%V
uri_parse("http://Uinfo@Host/myServer/line/one?Query#XyZ#Fragment-01", URI).	%V

uri_parse("mailto:Uinfo@HostX", URI).	%V
uri_parse("mailto:Uinfo@HostX.yz", URI).	%V
uri_parse("mailto:Uinfo@201.201.154", URI).	%V
uri_parse("mailto:Uinfo", URI).	%V

uri_parse("news:HostX", URI).	%V
uri_parse("news:HostX.yz", URI).	%V
uri_parse("news:201.201.154.255", URI).	%V

uri_parse("tel:Uinfo", URI).	%V
uri_parse("fax:Uinfo", URI).	%V

uri_parse("zos://Uinfo@HostX:85/ID44?QueryXyZ#Fragment-01", URI).	%V
uri_parse("zos://Uinfo@HostX:85/ID44val(id8Val)?QueryXyZ#Fragment-01", URI).	%V
uri_parse("zos://Uinfo@HostX:85/IDV.ID44val(id8Val)?QueryXyZ#Fragment-01", URI).	%V
uri_parse("zos://Uinfo@HostX:85/IDV44a.ID44b(id8Val)?QueryXyZ#Fragment-01", URI).	%V
uri_parse("zos://Uinfo@HostX:85/IDV.ID44val?QueryXyZ#Fragment-01", URI).    %V
uri_parse("zos://Uinfo@HostX.yz/ID44?QueryXyZ", URI).	%V
uri_parse("zos://Uinfo@200.200.201.203/ID44?QueryXyZ#Fragment-01", URI).	%V

uri_parse("mailtO:Uinfo@HostX", URI).	%V

%
%  Chiamate che devono generare errore 
%

uri_parse("http//Uinfo@HostX:85/myServer/line/one?QueryXyZ#Fragment-01", URI).	%V
uri_parse("http://Uinfo@HostX:85myServer/line/one?QueryXyZ#Fragment-01", URI).	%V
uri_parse("http:/Uinfo@HostX:85/myServer/line/one?QueryXyZ#Fragment-01", URI).	%V
uri_parse("http://Uinfo@/myServer/line/one", URI).	%V
uri_parse("http://Uinfo#a@HostX:85/myServer/line/one?QueryXyZ#Fragment-01", URI).	%V
uri_parse("http://Uinfo@Host@XXX:85/myServer/line/one?QueryXyZ#Fragment-01", URI).	%V
uri_parse("http://Uinfo@Host:/myServer/line/one?QueryXyZ#Fragment-01", URI).	%V
uri_parse("http://Uinfo@HostX.:85/myServer/line/one?QueryXyZ#Fragment-01", URI).	%V
uri_parse("http://Uinfo@HostX.A:8s/myServer/line/one?QueryXyZ#Fragment-01", URI).	%V
uri_parse("http://Uinfo@HostX/myServer/line/o@ne?QueryXyZ#Fragment-01", URI).	%V

uri_parse("news:HostsX/A", URI).	%V
uri_parse("news:HostsX:A", URI).	%V
uri_parse("mailto:Uinfo@HostX.", URI).	%V
uri_parse("mailto://Uinfo@HostX/myServer/line/one?QueryXyZ#Fragment-01", URI).	%V
uri_parse("mailto:Uinfo@HostX/myServer", URI).	%V
uri_parse("tel:Uinfo@HostX", URI).	%V
uri_parse("fax:@Uinfo", URI).	%V
uri_parse("fax:Uin@fo", URI).	%V


uri_parse("zos://Uinfo@", URI).	%V
uri_parse("zos://Uinfo@HostX.yz/", URI).	%V
uri_parse("zos://Uinfo@HostX:85/.?QueryXyZ#Fragment-01", URI).	%V
uri_parse("zos://Uinfo@HostX:85/.(id8Val)#Fragment-01", URI).	%V
uri_parse("zos://Uinfo@HostX:85/IDV(id8).ID44val(id8Val)?QueryXyZ#Fragment-01", URI).    %V
uri_parse("zos://Uinfo@HostX:85/IDV.ID44val.?QueryXyZ#Fragment-01", URI).    %V
uri_parse("zos://Uinfo@200.200.201.203/ID441234567890123456789012345678901234567890A?QueryXyZ#Fragment-01", URI).    %V
uri_parse("zos://Uinfo@HostX/1IDV.ID44val?QueryXyZ#Fragment-01", URI).    %V

%
% Esempi di uri_display
%
uri_display(uri(http, 'Uinfo', 'Host', 80, 'myServer/line/one', 'QueryXyZ','Fragment-01')).		%V
uri_display(uri(zos,'Uinfo', 'HostX', 85, 'IDV44a.ID44b(id8Val)', 'QueryXyZ', 'Fragment-01')).	%V