function transferCharacteristic5(ak,bk,m,n,M,N)

Nr = 10000;
Fs = 500;
f = (0:Fs/(Nr-1):Fs);
half = (1:Nr/2);

fn1 = (1+ak*exp(-1*i*2*pi*(f./Fs)*m)).^M;
fn2 = (1+bk*exp(-1*i*2*pi*(f./Fs)*n)).^N;
fn3 = fn1./fn2;

kk = (m/n)^M;
fn4 = exp(-1*i*2*pi*(f./Fs)*M*(m-n)/2);
fn5 = (fn4 - fn3/kk);

subplot(3,1,1);
plot(f(half), abs(fn3(half)));
subplot(3,1,2);
plot(f(half), abs(fn4(half)));
subplot(3,1,3);
plot(f(half), abs(fn5(half)));

figure;
half = (1:Nr/32);
plot(f(half), angle(fn5(half)));

end