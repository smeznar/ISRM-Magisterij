clear;
fprintf("Naloga 1: Aproksimacija funkcij z Bernsteinovimi polinomi in zveznimi odsekoma linearnimi funkcijami\n\n");
a = -1;
b = 1;
interval = linspace(0,1,201);
map = @(x) (b-a).*x+a;
f1 = @(x) cos(2*map(x));
f2 = @(x) (abs(map(x)).*cos(map(x).^2));
%f1 = @(x) cos(2*x);
%f2 = @(x) (abs(x).*cos(x.^2));
%% A
plot(interval,f1(interval)); hold on;
for k = 1:10
    bp = BersteinPolinom(f1,2*k);
    bv = bp(interval);
    plot(interval, bp(interval));
end

figure;
plot(interval,f2(interval)); hold on;
for k = 1:10
    bp = BersteinPolinom(f2,2*k);
    plot(interval, bp(interval));
end

%% B
figure;
plot(interval,f1(interval)); hold on;
for k = 1:10
    bp = OperatorI(f1,2*k, interval);
    plot(interval, bp);
end

figure;
plot(interval,f2(interval)); hold on;
for k = 1:10
    bp = OperatorI(f2,2*k, interval);
    plot(interval, bp);
end

%% C
format long;
ys1 = f1(interval);
anb = zeros(1,10);
ani = zeros(1,10);
for k = 1:10
    bp = BersteinPolinom(f1,2*k);
    ani(k) = norm(ys1-OperatorI(f1,2*k, interval),'inf');
    anb(k) = norm(f1(interval)-bp(interval),'inf');
end
bnb = zeros(1,10);
bni = zeros(1,10);
ys2 = f2(interval);
for k = 1:10
    bp = BersteinPolinom(f2,2*k);
    bni(k) = norm(ys2-OperatorI(f2,2*k, interval),'inf');
    bnb(k) = norm(f2(interval)-bp(interval),'inf');
end

%% D
format long;
red_anb = nan(1,10);
red_ani = nan(1,10);
red_bnb = nan(1,10);
red_bni = nan(1,10);
for i=2:10
    red_ani(i) = abs(log(ani(i)/ani(i-1))/log(i/(i-1)));
    red_bni(i) = abs(log(bni(i)/bni(i-1))/log(i/(i-1)));
    red_anb(i) = abs(log(anb(i)/anb(i-1))/log(i/(i-1)));
    red_bnb(i) = abs(log(bnb(i)/bnb(i-1))/log(i/(i-1)));
end

fprintf('f1(x)= cos(2*x)\n\n');

for i=1:10
    fprintf('n=%d\n',2*i);
    fprintf('Bernsteinov polinom: napaka = %f, red = %f\n',anb(i),red_anb(i));
    fprintf('Zvezna odsekoma linearna funkcija: napaka = %f, red = %f\n\n',ani(i),red_ani(i));
end

fprintf('f2(x)= (abs(x).*cos(x.^2))\n\n');

for i=1:10
    fprintf('n=%d\n',2*i);
    fprintf('Bernsteinov polinom: napaka = %f, red = %f\n',bnb(i),red_bnb(i));
    fprintf('Zvezna odsekoma linearna funkcija: napaka = %f, red = %f\n\n',bni(i),red_bni(i));
end
