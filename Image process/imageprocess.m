d = uigetdir(pwd, 'Select a folder');

scaned = dir(fullfile(d, '*.png'));

n_files = size(scaned,1);
thr = 100;
pixalarea = 0.0072;
nleaf = 0;

for i=1:n_files
	originalImage = imread([d,'\',scaned(i).name]);
    originalImage = rgb2gray(originalImage);
	bwimage = originalImage<thr;
	bwimage = imfill(bwimage, 'holes'); % fill the holes
    figure
    imshow(bwimage)
	labeled = bwlabel(bwimage, 8); % get label for each leaves
	coloredLabels = label2rgb (labeled, 'hsv', 'k', 'shuffle');  % colored labels
	figure
	imshow(coloredLabels);
	blobMeasurements = regionprops(labeled, originalImage, 'all');
	numberOfBlobs = size(blobMeasurements, 1);
	for j = 1:numberOfBlobs	
		blobArea = blobMeasurements(j).Area*pixalarea;		% Get area.
		blobPerimeter = blobMeasurements(j).Perimeter*(sqrt(pixalarea));		% Get perimeter.
		%blobCentroid = blobMeasurements(j).Centroid;		% Get centroid one at a time
		blobECD = sqrt(4 * blobArea / pi);					% Compute ECD - Equivalent Circular Diameter.
        if(blobArea>100000*pixalarea)
            nleaf = nleaf + 1
            result(nleaf,:)=[blobArea,blobPerimeter,blobECD];
        end
    end
end
colomes = {'Area_mm2','Perimeter_mm','ECD_mm'};
data = table(result(:,1),result(:,2),result(:,3),'VariableNames',colomes);
writetable(data, [d,'\','areastat.csv']);
