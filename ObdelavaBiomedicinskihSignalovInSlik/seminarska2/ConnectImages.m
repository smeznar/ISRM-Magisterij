function result = ConnectImages(Im1,Im2)
    offx = [ -1 -1 0 1 1 1 0 -1];
    offy = [ 0 -1 -1 -1 0 1 1 1];
    offx5 = [-2 -1 0 1 2 2 2 2 2 1 0 -1 -2 -2 -2 -2];
    offy5 = [-2 -2 -2 -2 -2 -1 0 1 2 2 2 2 2 1 0 -1];
    connectx = [-1 -1 0 1 1 1 1 1 1 1 0 -1 -1 -1 -1 -1];
    connecty = [-1 -1 -1 -1 -1 -1 0 1 1 1 1 1 1 1 0 -1];
    One = Im1 - ((Im1 == Im2) .* (Im1 == 1));
    %result = zeros(size(Im1));
    result = Im1;
    for i = 1:8
       One = One - ((One == circshift(Im2, [offy(1,i),offx(1,i)])) .* (One == 1)); 
    end
    for i = 1:length(offx5)
       shifted = circshift(Im2, [offy5(1,i),offx5(1,i)]);
       H = (One == shifted) .* (One == 1) .* (shifted == 1);
       % result(circshift(H, [offy5(1,i),offx5(1,i)])==1) = 1; % 3rd ring
       result(circshift(H, [connecty(1,i),connectx(1,i)])==1) = 1; % 2nd ring
       result(H==1) = 1; % 1st ring
    end
end

