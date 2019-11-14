function transferCharacteristic4(ak,bk,m,n,M,N)

Nr = 10000;
Fs = 500;
f = (0:Fs/(Nr-1):Fs);
half = (1:Nr/2);

fn1 = (1+ak*exp(-1*i*2*pi*(f./Fs)*m)).^M;
fn2 = (1+bk*exp(-1*i*2*pi*(f./Fs)*n)).^N;
fn3 = fn1./fn2;

subplot(3,1,1);
plot(f(half), abs(fn1(half)));
subplot(3,1,2);
plot(f(half), abs(fn2(half)));
subplot(3,1,3);
plot(f(half), abs(fn3(half)));

end