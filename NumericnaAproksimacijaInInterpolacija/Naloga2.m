clear;
fprintf('Naloga 2: Najboljša aproksimacija funkcije v enakomerni normi s (trigonometričnim) polinom po Remesovem postopku\n\n');
a = -1;
b = 1;
f = @(x) abs(x).*sin(2.*exp(1.5.*x)-1);
mn = [-1,-0.6,-0.2,0.2,0.6,1];
g1{1} = @(x) x.^4;
g1{2} = @(x) x.^3;
g1{3} = @(x) x.^2;
g1{4} = @(x) x;
g1{5} = @(x) 1;

fprintf('Priblizek za polinom najboljse enakomerne aproksimacije za f:\n');
interval = linspace(a,b,1001);
p4 = zeros(1,5);
u4 = zeros(1,5);
for i = 1:5
    coefs = Remes(f,a,b,mn,g1,i);
    p = @(x) 0;
    for j=1:length(g1)
            h = g1{j};
            p = @(x) p(x)+coefs(j).*h(x);
    end
    r = @(x) f(x) - p(x);
    p4(i) = norm(r(interval),'inf');
    u4(i) = p4(i)-coefs(end);
    fprintf('Napaka ujemanja na koraku %d je: %f.\n',i,u4(i));
end
fprintf('Ocena napake aproksimacije: %f\n\n', p4(i));


g2{1} = @(x) sin(2*x);
g2{2} = @(x) cos(2*x);
g2{3} = @(x) sin(x);
g2{4} = @(x) cos(x);
g2{5} = @(x) 1;

p2 = zeros(1,5);
u2 = zeros(1,5);
fprintf('Priblizek za trigonometricni polinom najboljse enakomerne aproksimacije za f::\n');
for i = 1:5
    coefs = Remes(f,a,b,mn,g2,i);
    q = @(x) 0;
    for j=1:length(g2)
            h = g2{j};
            q = @(x) q(x)+coefs(j).*h(x);
    end
    r = @(x) f(x)-q(x);
    p2(i) = norm(r(interval),'inf');
    u2(i) = p2(i)-coefs(end);
    fprintf('Napaka ujemanja na koraku %d je: %f.\n',i,u2(i));
end
fprintf('Ocena napake aproksimacije: %f\n\n', p2(i));

plot(f(interval)); hold on;
plot(p(interval))
plot(q(interval)); legend("f","p","q");
