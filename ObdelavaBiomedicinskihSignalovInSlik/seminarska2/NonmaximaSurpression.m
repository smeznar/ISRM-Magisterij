function [Imax] = NonmaximaSurpression(mag, angle)
    [h, w] = size(mag);
    Imax = zeros(h,w);
    offx = [ -1 -1 0 1];
    offy = [ 0 -1 -1 -1];
    for i = 0:3
        A = angle == i;
        index = i + 1;
        B = mag >= circshift(mag, [offy(1,index),offx(1,index)]);
        C = mag >= circshift(mag, [-offy(1,index),-offx(1,index)]);
        Imax = Imax + mag.*A.*B.*C;
    end
end

