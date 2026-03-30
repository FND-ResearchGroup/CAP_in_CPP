%% This code extracts you the CAPs measures and saves them in an excel file. 
% To run the statistics on the CAPs measures in R, the data must be in
% vertical shape.

% Inputs:
%       functfold er: Path to subject functional folder 
%
% Outputs:
%       X_flat: 2-dimensional data matrix containing [nVoxels x
%       nTimePoints]
%
% Created: Samantha Weber & Salome Häuselmann 

clear; clc;close;
addpath(genpath(pwd));

%% 1. Set parameters
RootPath= '';
CAPsPath = '';
CAPsfile = dir(fullfile(CAPsPath, 'x.mat')); 
load(fullfile(CAPsPath,CAPsfile.name),'Outputs'); 
filename = fullfile(CAPsPath,'x.xlsx');
nTP = 300; %300 volumes per subject

myGroups={'HC','CPP'};
% load P-codes HC;
filelist=dir(fullfile(RootPath,myGroups{1}, 'P*'));
N_HC=size(filelist,1);
myHC=cell(1,N_HC);
for f=1:N_HC
    myHC{f}=filelist(f).name;
end
clear filelist f

%load P-codes CPP
filelist=dir(fullfile(RootPath, myGroups{2},'P*'));
N_CD=size(filelist,1);
myCD=cell(1,N_CD);

for f=1:N_CD
    myCD{f}=filelist(f).name;
end
clear filelist f

mySubjects = [myHC,myCD];
N_HC = length(myHC); N_CD = length(myCD); nSubj=N_HC+N_CD; 

%% Extract CAPs Measures
% Make sure you read in the data correctly: As the code is now, it takes HC
% as reference population. If used CPP as reference population you have to
% switch the {1,1} to {1,2} and vice versa. 

%Extract Duration
Duration_HC = Outputs.Metrics.AverageExpressionDuration{1,1}(:,3:end-1); %rows are subjects, columns are states
Duration_HC(isnan(Duration_HC))=0; % replaces any NaN values in the extracted matrices with 0, ensuring there are no missing values.

Duration_CPP = Outputs.Metrics.AverageExpressionDuration{1,2}(:,3:end-1); %rows are subjects, columns are states
Duration_CPP(isnan(Duration_CPP))=0;

%Extract Entries
Entries_HC = Outputs.Metrics.NumberEntries{1,1}(:,3:end-1);
Entries_HC(isnan(Entries_HC))=0;

Entries_CPP = Outputs.Metrics.NumberEntries{1,2}(:,3:end-1);
Entries_CPP(isnan(Entries_CPP))=0;

nEntries_HC = sum(Entries_HC,2);

nEntries_CPP = sum(Entries_CPP,2);

%Extract Expression Duration
ExpressionDuration_HC = Outputs.Metrics.AverageExpressionDuration{1,1}(:,3:end-1);
ExpressionDuration_HC(isnan(ExpressionDuration_HC))=0;

ExpressionDuration_CPP = Outputs.Metrics.AverageExpressionDuration{1,2}(:,3:end-1);
ExpressionDuration_CPP(isnan(ExpressionDuration_CPP))=0;

%Extract Occurences
Occurences_HC = Outputs.Metrics.Occurrences{1,1}.raw.state(:,1:end-1);
Occurences_HC(isnan(Occurences_HC))=0;

Occurences_CPP = Outputs.Metrics.Occurrences{1,2}.raw.state(:,1:end-1);
Occurences_CPP(isnan(Occurences_CPP))=0;

notpicked_HC = Outputs.Metrics.Occurrences{1,1}.raw.notpicked(:,1);
notpicked_HC(isnan(notpicked_HC))=0;

scrubbed_HC = Outputs.Metrics.Occurrences{1,1}.raw.scrubbed(:,1);
scrubbed_HC(isnan(scrubbed_HC))=0;
nSelected_HC = nTP -(notpicked_HC + scrubbed_HC);

notpicked_CPP = Outputs.Metrics.Occurrences{1,2}.raw.notpicked(:,1);
notpicked_CPP(isnan(notpicked_CPP))=0;

scrubbed_CPP = Outputs.Metrics.Occurrences{1,2}.raw.scrubbed(:,1);
scrubbed_CPP(isnan(scrubbed_CPP))=0;
nSelected_CPP = nTP -(notpicked_CPP + scrubbed_CPP);


%Extract Resilience
CapResilience_HC = Outputs.Metrics.CAPResilience{1,1};
CapResilience_HC(isnan(CapResilience_HC))=0;

CapResilience_CPP = Outputs.Metrics.CAPResilience{1,2};
CapResilience_CPP(isnan(CapResilience_CPP))=0;

%Extract Baseline Resilience
BaselineResilience_HC = Outputs.Metrics.BaselineResilience{1,1};
BaselineResilience_HC(isnan(BaselineResilience_HC))=0;

BaselineResilience_CPP = Outputs.Metrics.BaselineResilience{1,2};
BaselineResilience_CPP(isnan(BaselineResilience_CPP))=0;

%Extract Entries from Baseline
CAPEntriesFromBaseline_HC = Outputs.Metrics.CAPEntriesFromBaseline{1,1};
CAPEntriesFromBaseline_HC(isnan(CAPEntriesFromBaseline_HC))=0;

CAPEntriesFromBaseline_CPP = Outputs.Metrics.CAPEntriesFromBaseline{1,2};
CAPEntriesFromBaseline_CPP(isnan(CAPEntriesFromBaseline_CPP))=0;

%Extract Exit to Baseline
CAPExitToBaseline_HC = Outputs.Metrics.CAPExitsToBaseline{1,1};
CAPExitToBaseline_HC(isnan(CAPExitToBaseline_HC))=0;

CAPExitToBaseline_CPP = Outputs.Metrics.CAPExitsToBaseline{1,2};
CAPExitToBaseline_CPP(isnan(CAPExitToBaseline_CPP))=0;

%Extract Transition
Transition_HC = Outputs.Metrics.TransitionProbabilities{1,1}(3:end-1,3:end-1,:);
Transition_HC(isnan(Transition_HC))=0;

Transition_CPP = Outputs.Metrics.TransitionProbabilities{1,2}(3:end-1,3:end-1,:);
Transition_CPP(isnan(Transition_CPP))=0;

%Extract Number of excluded frames (based on FD)
ScrubbedActiveFrames_HC = Outputs.SpatioTemporalSelection.Indices{1, 1}.scrubbedandactive;
ScrubbedActiveFrames_HC(isnan(ScrubbedActiveFrames_HC))=0;
ScrubbedActiveFrames_HC = sum(ScrubbedActiveFrames_HC, 1);
ExcludedFrames_HC = ScrubbedActiveFrames_HC.';  % Transpose

ScrubbedActiveFrames_CPP = Outputs.SpatioTemporalSelection.Indices{1, 2}.scrubbedandactive;
ScrubbedActiveFrames_CPP(isnan(ScrubbedActiveFrames_CPP))=0;
ScrubbedActiveFrames_CPP = sum(ScrubbedActiveFrames_CPP, 1);
ExcludedFrames_CPP = ScrubbedActiveFrames_CPP.';  % Transpose

%Extract Number of selected frames
SelectedFrames_HC = Outputs.SpatioTemporalSelection.Indices{1, 1}.kept.active;
SelectedFrames_HC(isnan(SelectedFrames_HC))=0;
SelectedFrames_HC = sum(SelectedFrames_HC, 1);
SelectedFrames_HC = SelectedFrames_HC.';  % Transpose

SelectedFrames_CPP = Outputs.SpatioTemporalSelection.Indices{1, 2}.kept.active;
SelectedFrames_CPP(isnan(SelectedFrames_CPP))=0;
SelectedFrames_CPP = sum(SelectedFrames_CPP, 1);
SelectedFrames_CPP = SelectedFrames_CPP.';  % Transpose

%Extract Number of retained frames (absolute)
RetainedFrames_HC = Outputs.SpatioTemporalSelection.RetainedFramesPerSeed{1, 1};
RetainedFrames_HC(isnan(RetainedFrames_HC))=0;
RetainedFrames_HC = sum(RetainedFrames_HC, 1);
RetainedFrames_HC = RetainedFrames_HC.';  % Transpose

RetainedFrames_CPP = Outputs.SpatioTemporalSelection.RetainedFramesPerSeed{1, 2};  
RetainedFrames_CPP(isnan(RetainedFrames_CPP))=0;
RetainedFrames_CPP = sum(RetainedFrames_CPP, 1);
RetainedFrames_CPP = RetainedFrames_CPP.';  % Transpose

%Extract Number of retained frames (percentage)
RelativeRetainedFrames_HC = Outputs.SpatioTemporalSelection.PercentageRetainedFrames{1, 1};
RelativeRetainedFrames_HC(isnan(RelativeRetainedFrames_HC))=0;
RelativeRetainedFrames_HC = RelativeRetainedFrames_HC.';  % Transpose

RelativeRetainedFrames_CPP = Outputs.SpatioTemporalSelection.PercentageRetainedFrames{1, 2};  
RelativeRetainedFrames_CPP(isnan(RelativeRetainedFrames_CPP))=0;
RelativeRetainedFrames_CPP = RelativeRetainedFrames_CPP.';  % Transpose


%% Define Number of States

nStates = size(Occurences_HC,2);
namesStates = {};
namesSubj = {};

%CAPs header
for i = 1:nStates
    for s = 1:nSubj
        thisState{s,1} = ['CAP ' num2str(i)];
    end
        namesStates = [namesStates; thisState];
        namesSubj = [namesSubj; mySubjects'];
end
clear thisState i s

header = {'ID','State','value'};

%Baseline header

for s = 1:nSubj
    namesBaseline{s,1} = 'Baseline';
end
namesSubj_Baseline = mySubjects';

clear  s

%Control header

for k = 1:nSubj
    namesControl{k,1} = 'Control';
end
namesSubj_Control = mySubjects';

clear  k


%% Merge it all together to export to excel
% Output will be an excel file in columns

% Duration
Duration =[];
for i = 1:nStates
    thisDuration = [num2cell(Duration_HC(:,i)); num2cell(Duration_CPP(:,i))];
    Duration = [Duration; thisDuration];
end
Duration = [header; namesSubj namesStates Duration];
clear i thisDuration

% Entries
Entries = [];
for i = 1:nStates
    thisEntry = [num2cell(Entries_HC(:,i)); num2cell(Entries_CPP(:,i))];
    Entries = [Entries; thisEntry];
end
Entries = [header; namesSubj namesStates Entries];
clear i thisEntry

% Relative Entries (compared to total entries)
Rel_Ent_raw = [Entries_HC; Entries_CPP];
total = [nEntries_HC; nEntries_CPP];
RelativeEntries_t = [];
for i =1:nStates
    for s = 1:nSubj
        thisRelativeEntries_t(s,1) = (Rel_Ent_raw(s,i)/total(s,1));
    end
    RelativeEntries_t = [RelativeEntries_t; num2cell(thisRelativeEntries_t)];
end
clear thisRelOccurence s
RelativeEntries_final_t = [header; namesSubj namesStates RelativeEntries_t];
clear i

% Relative Entries (compared to RetainedFrames)
Rel_Ent_raw = [Entries_HC; Entries_CPP];
nSelected = [nSelected_HC; nSelected_CPP];
total = [nEntries_HC; nEntries_CPP];
RelativeEntries_r = [];
for i =1:nStates
    for s = 1:nSubj
        thisRelativeEntries_r(s,1) = (Rel_Ent_raw(s,i)/nSelected(s,1));
    end
    RelativeEntries_r = [RelativeEntries_r; num2cell(thisRelativeEntries_r)];
end
clear thisRelativeEntries_r s
RelativeEntries_final_r = [header; namesSubj namesStates RelativeEntries_r];
clear i

% Average Expression Duration
ExpressionDuration = [];
for i = 1:nStates
    thisExpressionDuration = [num2cell(ExpressionDuration_HC(:,i)); num2cell(ExpressionDuration_CPP(:,i))];
    ExpressionDuration = [ExpressionDuration; thisExpressionDuration];
end
ExpressionDuration = [header; namesSubj namesStates ExpressionDuration];
clear i thisExpressionDuration

% Occurence
Occurence =[];
for i = 1:nStates
    thisOccurence = [num2cell(Occurences_HC(:,i)); num2cell(Occurences_CPP(:,i))];
    Occurence = [Occurence; thisOccurence];
end
Occurence = [header; namesSubj namesStates Occurence];
clear i thisOccurence


% Relative Occurence
Rel_Occ_raw = [Occurences_HC; Occurences_CPP];
nSelected = [nSelected_HC; nSelected_CPP];
RelativeOccurence = [];
for i =1:nStates
    for s = 1:nSubj
        thisRelOccurence(s,1) = (Rel_Occ_raw(s,i)/nSelected(s,1));
    end
    RelativeOccurence = [RelativeOccurence; num2cell(thisRelOccurence)];
end
clear thisRelOccurence s
RelativeOccurence_final = [header; namesSubj namesStates RelativeOccurence];
clear i  


%Resilience
CapResilience =[];
for i = 1:nStates
    thisCapResilience = [num2cell(CapResilience_HC(:,i)); num2cell(CapResilience_CPP(:,i))];
    CapResilience = [CapResilience; thisCapResilience];
end
CapResilience = [header; namesSubj namesStates CapResilience];
clear i thisCapResilience

%Baseline Resilience
BaselineResilience =[];
%for i = 1:nStates
    thisBaselineResilience = [num2cell(BaselineResilience_HC); num2cell(BaselineResilience_CPP)];
    BaselineResilience = [BaselineResilience; thisBaselineResilience];
%end
BaselineResilience = [header; namesSubj_Baseline namesBaseline BaselineResilience];
clear i thisBaselineResilience

%EntriesFrom Baseline
CapEntriesBaseline =[];
for i = 1:nStates
    thisCapEntriesBaseline = [num2cell(CAPEntriesFromBaseline_HC(:,i)); num2cell(CAPEntriesFromBaseline_CPP(:,i))];
    CapEntriesBaseline = [CapEntriesBaseline; thisCapEntriesBaseline];
end
CapEntriesBaseline = [header; namesSubj namesStates CapEntriesBaseline];
clear i thisCapEntriesBaseline

%Exits to Baseline
CapExitsBaseline =[];
for i = 1:nStates
    thisCapExitsBaseline = [num2cell(CAPExitToBaseline_HC(:,i)); num2cell(CAPExitToBaseline_CPP(:,i))];
    CapExitsBaseline = [CapExitsBaseline; thisCapExitsBaseline];
end
CapExitsBaseline = [header; namesSubj namesStates CapExitsBaseline];
clear i thisCapExitsBaseline

Transition_HC = [num2cell(mean(Transition_HC,3))];
Transition_CPP = [num2cell(mean(Transition_CPP,3))];

% Number of excluded frames (based on FD)
ExcludedFrames =[];
thisExcludedFrames = [num2cell(ExcludedFrames_HC); num2cell(ExcludedFrames_CPP)];
ExcludedFrames = [ExcludedFrames; thisExcludedFrames];

ExcludedFrames = [header; namesSubj_Control namesControl ExcludedFrames];

% Number of selected frames
SelectedFrames =[];
thisSelectedFrames = [num2cell(SelectedFrames_HC); num2cell(SelectedFrames_CPP)];
SelectedFrames = [SelectedFrames; thisSelectedFrames];

SelectedFrames = [header; namesSubj_Control namesControl SelectedFrames];

% Retained-frames (absolut)
RetainedFrames =[];
thisRetainedFrames = [num2cell(RetainedFrames_HC); num2cell(RetainedFrames_CPP)];
RetainedFrames = [RetainedFrames; thisRetainedFrames];

RetainedFrames = [header; namesSubj_Control namesControl RetainedFrames];

% Relative Retained-frames 
RelRetainedFrames =[];
thisRelRetainedFrames = [num2cell(RelativeRetainedFrames_HC); num2cell(RelativeRetainedFrames_CPP)];
RelRetainedFrames = [RelRetainedFrames; thisRelRetainedFrames];

RelRetainedFrames = [header; namesSubj_Control namesControl RelRetainedFrames];

%% Write it to excel
% Write excel file
writecell(Duration,filename,'Sheet','Duration');
writecell(Occurence, filename,'Sheet','Occurence');
writecell(RelativeOccurence_final,filename,'Sheet','RelativeOccurence');
writecell(Entries,filename,'Sheet','Entries');
writecell(RelativeEntries_final_t,filename,'Sheet','RelativeEntries_t');
writecell(RelativeEntries_final_r,filename,'Sheet','RelativeEntries_r');
writecell(CapResilience, filename,'Sheet','CapResilience');
writecell(BaselineResilience,filename,'Sheet','BaselineResilience');
writecell(ExpressionDuration,filename,'Sheet','ExpressionDuration');
writecell(CapEntriesBaseline, filename,'Sheet','EntriesFromBaseline');
writecell(CapExitsBaseline, filename,'Sheet','ExitsToBaseline');
writecell(Transition_HC,filename,'Sheet','Transition_HC');
writecell(Transition_CPP, filename,'Sheet','Transition_CPP');
writecell(ExcludedFrames, filename,'Sheet','ExcludedFrames');
writecell(SelectedFrames, filename,'Sheet','SelectedFrames');
writecell(RetainedFrames, filename,'Sheet','RetainedFrames');
writecell(RelRetainedFrames, filename,'Sheet','RelRetainedFrames');