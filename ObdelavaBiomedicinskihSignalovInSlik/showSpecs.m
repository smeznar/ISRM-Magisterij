function Y = showSpecs(signal, Fs)

  n = length(signal);
  f = (((0:n-1))/n)*Fs; 
  
  % Fourier transform
  Y = fft(signal);

  half = (1:n/2);
  
%  figure; % real part of Fourier transform 
%  plot(f,real(Y));
  
%  figure; % complex part of Fourier transform
%  plot(f,imag(Y));
  
 % amplitude spectrum
 figure;
 plot(f(half), abs(Y(half))); 
 
 % power spectrum
 figure;
 plot(f(half), (1/n)*(abs(Y(half))./n).^2);

 % phase spectrum
 figure;
 plot(f(half), angle(Y(half)));
 
end





