function [idx] = evaldetect(fileName)
    % First set coefficients for H1 (assume Fs=250 Hz)
    m = 6; n = 5;
    a1 = [1, -1];
    h1a = [1, zeros(1,m-1), -1]; 
    h1b = [1, zeros(1,n-1), -1];
    b1 = conv(h1a, h1b);
    
    % Now set coefficients for H2 (assume Fs=250 Hz)
    m = 5; n = 5; 
    a2 = [1, -1];
    h2a = [1, zeros(1,m-1), -1]; 
    h2b = [1, zeros(1,n-1), -1];
    b2 = conv( conv(h2a, h2a), h2b);
    
    % For exercise also compute the impulse response of both filters
    h1 = impz(b1,a1)';
    h2 = impz(b2,a2)';
    
    % Load record and store into variables
    load(fileName);
    sig = val;
    leads = size(sig, 1); % number of leads 
    sigLen = size(sig, 2); % signal length in samples
    sigX = 1:sigLen; % vector for x-axis (in samples)
    
    % Zero-out output vector (keep dimensions)
    sumh = zeros(1,sigLen);
    % For each lead compute abs(H1) and H2
    for i=1:leads
        h1 = filter(b1,a1,sig(i,:));
        h2 = filter(b2,a2,sig(i,:));
        sumh = sumh + (abs(h1) + abs(h2));
    end
    % Square the sum
    sumhsquared = sumh.^2;
    
    % Final moving average / smoothing filter G
    k = 10;
    gb = [1, zeros(1,k-1), -1];
    ga = [1, -1];
    sumfinal = filter(gb,ga,sumhsquared/k);
    
    % Plot results, but take into account the filter delays
    GAIN = 1; % Scale the input signal if shown on the same plot
    hold on;
    subplot(3,1,1);
    plot(sigX, sig(1,:) * GAIN, '-.', 'Color', [0.6,0.6,0.6]);
    subplot(3,1,2);
    plot(sigX, sumhsquared, 'b'); 
    subplot(3,1,3);
    plot(sigX, sumfinal, 'k'); 
    hold off;

    % ----- IMPLEMENT DECISION RULE HERE (BEGIN) -----
    % i.e. detect the heart beats, start at the beginning
    idx = []; % holds indexes of detected heart beats
    for i=1:sigLen
       % if heart beat (peak) is detected add the index to vector
       % either with idx=[idx,i] or idx=[idx,i-D1-D2];
    end
    % ----- IMPLEMENT DECISION RULE HERE (END) -----

    % Function returns vector of detected indexes
end
