clear

figure
load fort.1003
nx = 100;
nv = nx;
x = fort(:,1);
y = fort(:,2);
z = fort(:,3);
x = reshape(x, nx, nv);
y = reshape(y, nx, nv);
z = reshape(z, nx, nv);
mesh(x, y, z)
colormap jet;
colorbar;
xlabel('x', 'fontsize', 24)
ylabel('y', 'fontsize', 24)
zlabel('u', 'fontsize', 24)
% axis([0, 2, 0, 2, 0, 1.])
set(gca,'FontSize',18)
title('(1,2,4,3)', 'FontSize',18)
%    print -dpng 1234_rk3_t01_cfl48.png

figure
pcolor(x, y, z);
colormap jet;
colorbar;
xlabel('X', 'fontsize', 24)
ylabel('Y', 'fontsize', 24)
% axis([0, 2, 0, 2, 0, 1.])
set(gca,'FontSize',18)
title('(1,2,4,3)', 'FontSize', 18)
shading interp
% print -dpng 1234_rk3_t01_cfl48_pcolor.png
% 
% figure
% load fort.2001
% x = fort(:,1);
% y = fort(:,2);
% plot(x, y, '-k')
% hold on
% load fort.1001
% x = fort(:,1);
% y = fort(:,2);
% plot(x, y, '+r')
% %title('Method I, X = 0', 'FontSize', 18)
% title('X = 0', 'FontSize', 18)
% print -dpng rotation_1_slide1.png

% figure
% load fort.2002
% x = fort(:,1);
% y = fort(:,2);
% plot(x, y, '-k')
% hold on
% load fort.1002
% x = fort(:,1);
% y = fort(:,2);
% plot(x, y, '+r')
% %title('Method I, Y = -1.6', 'FontSize', 18)
% title('Y = -1.6', 'FontSize', 18)
% print -dpng rotation_1_slide2.png
% 
% figure
% load fort.2004
% x = fort(:,1);
% y = fort(:,2);
% plot(x, y, '-k')
% hold on
% load fort.1004
% x = fort(:,1);
% y = fort(:,2);
% plot(x, y, '+r')
% %title('Method I, Y = 1.54', 'FontSize', 18)
% title('Y = 1.54', 'FontSize', 18)
% print -dpng rotation_1_slide4.png