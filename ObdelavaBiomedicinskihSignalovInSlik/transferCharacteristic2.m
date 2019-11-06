function transferCharacteristic2(ck,j,N)

Nr = 1000;
Fs = 500;
f = (0:Fs/(Nr-1):Fs);
half = (1:Nr/2);

fn1 = (1+ck*exp(-1*i*2*pi*(f./Fs)*j)).^N;

plot(f(half), abs(fn1(half)));

end
