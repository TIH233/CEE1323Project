clc
close all
clear all
load Train_binary.txt
load Test_binary.txt
Xtrain = Train_binary(:, 1:4);
Ytrain = Train_binary(:, 5);
Xtest = Test_binary(:, 1:4);
Ytest = Test_binary(:, 5);

opts = struct('AcquisitionFunctionName','expected-improvement-plus','ShowPlots',true);
Model = fitcsvm(Xtrain,Ytrain,'KernelFunction','rbf',...
    'OptimizeHyperparameters','auto','HyperparameterOptimizationOptions',opts)

% 'fitcecoc' for multiclass, it uses K(K – 1)/2 binary support vector machine (SVM) models using the one-versus-one coding design,  

% Notes: The SVM model can be set using "KernelScale" like this "Model = fitcsvm(..., 'KernelScale', 1/(2*sqrt(gamma));"
%     


%'KernelFunction', 'rbf', 'linear', and 'polynomial'
%     'PolynomialOrder', 2, ...
%     'BoxConstraint', 1, ...   %%the Langrange multipliers are bounded to be within the range [0,C]
%     'KernelScale', 'auto',...
%     'Standardize', true, 
% Good to standardize the predictors



% Check https://www.mathworks.com/help/stats/fitcsvm.html
% Also here for BoxConstraint: https://stackoverflow.com/questions/31161075/svm-in-matlab-meaning-of-parameter-box-constraint-in-function-fitcsvm/31171332

%BoxConstraint: It is called C elsewhere. A box constraint is a parameter that controls the maximum 
%penalty imposed on margin-violating observations,which helps to prevent overfitting (regularization).
%If you increase the box constraint, then the SVM classifier assigns fewer support vectors. 
%Increasing the box constraint can lead to longer training times.
% A large C gives you low bias and high variance. High accuracy but overftting.  
% A small C gives you higher bias and lower variance.

% 'OptimizeHyperparameters' 
% 'none' — Do not optimize.
% 'auto' — Use {'BoxConstraint','KernelScale'}.
% 'all' — Optimize all eligible parameters.

%Plot confusion matrix
figure(3);
Predict_train = predict(Model,Xtrain);
Predict_test = predict(Model,Xtest);
plotconfusion(Ytrain',Predict_train','Training Data')
textobj = findobj('type', 'text');
set(textobj, 'fontunits', 'points','fontsize', 11, 'fontweight', 'bold');

figure(4);
plotconfusion(Ytest',Predict_test','Testing Data')
textobj = findobj('type', 'text');
set(textobj, 'fontunits', 'points','fontsize', 11, 'fontweight', 'bold');


%%%%%% Receiver operating characteristic (ROC)
figure(5);
[~,score_test]= predict(Model,Xtest); % Compute the posterior probabilities (scores).
[X,Y,T,AUC] = perfcurve(Ytest,score_test(:,2),'1');
plot(X,Y)
xlabel('False positive rate') 
ylabel('True positive rate')
title('ROC for Classification (Testing Data)')




