clear;
u25=zeros(25,25);
u_temp= load('fort.1');
k=1;
for i = 1:25;
    for j = 1:25;
       u25(i,j) =  u_temp(k);
       k=k+1;
    end
end


clear u_temp;
u_temp= load('fort.2');
u50 = zeros(50,50);
k=1;
for i = 1:50;
    for j = 1:50;
       u50(i,j) =  u_temp(k);
       k=k+1;
    end
end
clear i j k;
%----------------------------------------------
u_exact = zeros(25,25);
error1_25 = 0;
error2_25 = 0;
for i = 1:25;
    for j = 1:25;
        u_exact(i,j) = (u50(2*i-1,2*j-1)+u50(2*i,2*j-1)+u50(2*i-1,2*j)+u50(2*i,2*j))/4;
        error1_25 = error1_25 + abs(u25(i,j)-u_exact(i,j));
        error2_25 = error2_25 + (u25(i,j)-u_exact(i,j)).^2;
    end
end
error1_25 = error1_25/(25*25);
error2_25 = sqrt(error2_25/(25*25));
clear i j;
%------------------------------
clear u_temp;
u_temp= load('fort.3');
u100 = zeros(100,100);
k=1;
for i = 1:100;
    for j = 1:100;
       u100(i,j) =  u_temp(k);
       k=k+1;
    end
end
%-----------------------------------------
clear u_exact;
u_exact = zeros(50,50);
error1_50 = 0;
error2_50 = 0;
for i = 1:50;
    for j = 1:50;
        u_exact(i,j) = (u100(2*i-1,2*j-1)+u100(2*i,2*j-1)+u100(2*i-1,2*j)+u100(2*i,2*j))/4;
        error1_50 = error1_50 + abs(u50(i,j)-u_exact(i,j));
        error2_50 = error2_50 + (u50(i,j)-u_exact(i,j)).^2;
    end
end
error1_50 = error1_50/(50*50);
error2_50 = sqrt(error2_50/(50*50));
clear i j;
%------------------------------------------------

clear u_temp;
u_temp= load('fort.4');
u200 = zeros(200,200);
k=1;
for i = 1:200;
    for j = 1:200;
       u200(i,j) =  u_temp(k);
       k=k+1;
    end
end
clear i j k;

%-----------------------------------------
clear u_exact;
u_exact = zeros(100,100);
error1_100 = 0;
error2_100 = 0;
for i = 1:100;
    for j = 1:100;
        u_exact(i,j) = (u200(2*i-1,2*j-1)+u200(2*i,2*j-1)+u200(2*i-1,2*j)+u200(2*i,2*j))/4;
        error1_100 = error1_100 + abs(u100(i,j)-u_exact(i,j));
        error2_100 = error2_100 + (u100(i,j)-u_exact(i,j)).^2;
    end
end
error1_100 = error1_100/(100*100);
error2_100 = sqrt(error2_100/(100*100));
clear i j;
%------------------------------------------------

clear u_temp;
u_temp= load('fort.100');
u400 = zeros(400,400);
k=1;
for i = 1:400;
    for j = 1:400;
       u400(i,j) =  u_temp(k);
       k=k+1;
    end
end
clear i j k;

%-----------------------------------------
clear u_exact;
u_exact = zeros(200,200);
error1_200 = 0;
error2_200 = 0;
for i = 1:200;
    for j = 1:200;
        u_exact(i,j) = (u400(2*i-1,2*j-1)+u400(2*i,2*j-1)+u400(2*i-1,2*j)+u400(2*i,2*j))/4;
        error1_200 = error1_200 + abs(u200(i,j)-u_exact(i,j));
        error2_200 = error2_200 + (u200(i,j)-u_exact(i,j)).^2;
    end
end
error1_200 = error1_200/(200*200);
error2_200 = sqrt(error2_200/(200*200));
clear i j;
%------------------------------------------------
    
   
l1 = [ordercal(error1_25,error1_50),ordercal(error1_50,error1_100),ordercal(error1_100,error1_200)]
l2 = [ordercal(error2_25,error2_50),ordercal(error2_50,error2_100),ordercal(error2_100,error2_200)]
    
%     
% l1 = [ordercal(error1_25,error1_50),ordercal(error1_50,error1_100)]
% l2 = [ordercal(error2_25,error2_50),ordercal(error2_50,error2_100)]
    
    
    