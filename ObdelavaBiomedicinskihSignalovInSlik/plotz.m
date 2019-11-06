clear all;

b = 1;
a = [1 1/6 -1/18];
% figure; zplane(b,a);

clear all;
[x,y] = meshgrid([-1:0.01:1]);
z = x+j*y;
H = 1 ./ ( 1 + 1/6.*z.^-1 - 1/18.*z.^-2);
figure; surf(x,y,abs(H));

figure; 
imagesc(x(1,:),y(:,1),abs(H),[0,2]); hold on;
% and plot a black circle with r=1
x=[-1:.01:1]; y=[sqrt(1-x.^2);-sqrt(1-x.^2)];
plot(x,y,'k'); axis square;

%{
h = @(z) 1/(1+1/6*z^-1-1/18*z^-2);
x = -1:0.01:1;
y = (-1:0.01:1);

for n = 1:length(y)
    for m = 1:length(x)
        H(n,m)=h( x(m) + j*y(n) );
    end
end

Ha = abs(H);
figure; surf(x,y,Ha,gradient(Ha));
% add labels to axis
xlabel('Real');
ylabel('Imaginary');
zlabel('Amplitude');
%}