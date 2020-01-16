function ScanImages(directory)
    dirs = dir(directory+"*.png");
    original = cell(length(dirs),1);
    pics = cell(length(dirs),1);
    connections = cell(length(dirs),1);
    for k=1:length(dirs)
        picdir = strcat(directory,dirs(k).name);
        original{k} = im2double(imread(picdir));
        pics{k} = Canny(original{k});
    end
    for k=2:length(dirs)
       connections{k-1} = ConnectImages(pics{k-1},pics{k});
    end
    connections{k} = zeros(size(pics{k}));
    figure('Renderer', 'painters', 'Position', [10 10 1200 700])
    for k=1:length(dirs)
        subplot(1,3,1)
        imshow(original{k});
        subplot(1,3,2)
        imshow(pics{k});
        subplot(1,3,3)
        imshow(connections{k})
        imwrite(connections{k}, strcat("saved_images/edited_",dirs(k).name));
        waitforbuttonpress;
    end
end

