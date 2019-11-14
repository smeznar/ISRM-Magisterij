lwFreq = 0.8 / (250.0 / 2);
x = 1:length(sig);
allN = [10 100 200 300 600 800 1000 1200];
intr = x;
% intr = 11000:12500;
for i=1:length(allN)
	n = allN(i);
	b = fir1(n, lwFreq, 'high');
	flt = filter(b,1,sig);
	clf; % Clear figure or create new figure
	labels = {'Unfiltered'; sprintf('Filtered, n=%d', n)};
	plot(x(intr), sig(intr));
	hold on;
	plot(x(intr), flt(intr), 'r');
	legend(labels);
	strg = input('Press a key ...');
end
