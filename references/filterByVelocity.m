function [stats,out] = filterByVelocity(options, locs,dataLegend)%xy, time, maxV, nPointsReliability,timeDifference)
    
stats.function = mfilename;
stats.timeDifs = []; 
stats.fractionAfterVelocityFilter=nan(1,1);
tic

out=locs;

VELOCITY_UPPER_BOUND=options.v_filter_max_v;
NUMBER_OF_GOOD_POINTS_NEEDED_FOR_RELIABILITY=options.min_time_dif_for_stats;%5; 
TIME_BOUND=options.v_filter_time_dif;

%^for ti=1:length(locs)
%ti
    xy=locs(:,dataLegend.x:dataLegend.y);
    time=locs(:,dataLegend.time);

    dists=calDists(xy(2:end,:),xy(1:end-1,:));
    timeDifs=time(2:end)-time(1:end-1);
    
    stats.timeDifs=[stats.timeDifs,timeDifs(timeDifs>options.min_time_dif_for_stats)'];
    if size(dists)==size(timeDifs)
    velocities=dists./timeDifs;
    else velocities=0;
    end
    if sum(velocities<0)>1
        disp('WARNING: time seems to not be in ascending order, some negative velocities found.');
        disp(['# such points: ',num2str(sum(velocities<0))]);
    end
    reachedEnd=false;
    reliablePoints=false(length(xy),1);
    i=reliable(velocities,timeDifs);
   
    if isempty(i)
        disp('WARNING: no first reliable point found, check parameters');
        reachedEnd=true;
    else
        reliablePoints(i)=true;
    end
    while ~reachedEnd
        for j=i+1:length(xy)
            if j==length(xy)
                reachedEnd=true;
            end

            timeDifference=time(j)-time(i);
            if timeDifference>=TIME_BOUND
                i=j-1+reliable(velocities(j:end),timeDifs(j:end));
                if isempty(i)
                    reachedEnd=true;
                else
                    reliablePoints(i)=true;
                end
                break
            else
                distance=calDists(xy(i,:),xy(j,:));
                velocity=distance/timeDifference;
                if velocity<VELOCITY_UPPER_BOUND
                    i=j;
                    reliablePoints(i)=true;
                    break
                end
            end
        end
    end
    stats.fractionAfterVelocityFilter(i)=sum(reliablePoints)/size(xy,1);
    out=out(reliablePoints,:);

%end

stats.time = toc;

function reliableInd = reliable(relevantVelocities,relevantTimeDifs)
    if length(relevantVelocities)<NUMBER_OF_GOOD_POINTS_NEEDED_FOR_RELIABILITY
        reliableInd=[];
    else
        inds=true(length(relevantVelocities),1);
        for k=1:NUMBER_OF_GOOD_POINTS_NEEDED_FOR_RELIABILITY
            inds=inds&[relevantVelocities(k:end);ones(k-1,1).*VELOCITY_UPPER_BOUND]<VELOCITY_UPPER_BOUND&...
                [relevantTimeDifs(k:end);ones(k-1,1).*TIME_BOUND]<TIME_BOUND;
        end
        reliableInd=find(inds,1);
    end
end

end