function [mag,angle] = DeriveImage(I)
    sobely = [-1,-2,-1;0,0,0;1,2,1];
    %sobely = sobely/norm(sobely);
    sobelx = [-1,0,1;-2,0,2;-1,0,1];
    %sobelx = sobelx/norm(sobelx);
    kernel = fspecial('gaussian',[7 7],1.0);
    ci = imfilter (I, kernel);
    gx = imfilter (double(ci), sobelx);
    gy = imfilter (double(ci), sobely);
    mag = sqrt((gx.^2) + (gy.^2));
    angle = mod (round(((atan(gy./gx)+pi)./pi)*4), 4) ;
end

