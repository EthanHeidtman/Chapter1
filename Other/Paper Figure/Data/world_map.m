ax = worldmap("World");
land = readgeotable("landareas.shp");
geoshow(ax,land,"FaceColor",[0.5 0.7 0.5])
%geoshow(cities,"Marker",".","Color","red");
rivers = readgeotable("worldrivers.shp");
geoshow(rivers,"Color","blue")
lakes = readgeotable("worldlakes.shp");
geoshow(lakes,"FaceColor","blue")
[NUM,TXT,RAW]=xlsread('Rivers_2.xlsx');
 lat=NUM(:,2)+NUM(:,3)/60+NUM(:,4)/3600;
 lon=NUM(:,6)+NUM(:,7)/60+NUM(:,8)/3600;
 lat=NUM(:,1).*lat;
 lon=NUM(:,5).*lon;

 n=length(lon);

 for i=1:n;
     ax=geoshow(lat(i),lon(i));
     set(ax,'Marker','o','MarkerFaceColor','r')
 end

