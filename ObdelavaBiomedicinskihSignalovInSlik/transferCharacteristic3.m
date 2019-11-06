function transferCharacteristic3(ck,j,N,m,M)

Nr = 1000;
Fs = 500;
f = (0:Fs/(Nr-1):Fs);
half = (1:Nr/2);

fn1 = (1-exp(-1*i*2*pi*(f./Fs)*m)).^M;
fn2 = (1-exp(-1*i*2*pi*(f./Fs))).^M;
fn3 = (1+ck*exp(-1*i*2*pi*(f./Fs)*j)).^N;

fn4 = fn1./fn2;
fn5 = fn4./fn3;

subplot(5,1,1);
plot(f(half), abs(fn1(half)));
subplot(5,1,2);
plot(f(half), abs(fn2(half)));
subplot(5,1,3);
plot(f(half), abs(fn3(half)));
subplot(5,1,4);
plot(f(half), abs(fn4(half)));
subplot(5,1,5);
plot(f(half), abs(fn5(half)));

end