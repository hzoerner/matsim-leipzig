package org.matsim.analysis;

import com.google.inject.Inject;
import org.apache.log4j.Logger;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.population.Leg;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.core.router.AnalysisMainModeIdentifier;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

public class LeipzigMainModeIdentifier implements AnalysisMainModeIdentifier {
    private final List<String> modeHierarchy = new ArrayList<>();
    private final List<String> drtModes = Arrays.asList("drtLeutzsch","drtSouthwest","drtNorth","drtSoutheast","drt", "av",
            "drt_teleportation","drt1","drt2","drt3","drt4","drt5","drt6","drt7","drt8","drt9","drt10","drt11","drt12","drt13","drt14","drt15");
    public static final String ANALYSIS_MAIN_MODE_PT_WITH_DRT_USED_FOR_ACCESS_OR_EGRESS = "pt_w_drt_used";
    private static final Logger log = Logger.getLogger(LeipzigMainModeIdentifier.class);

    @Inject
    public LeipzigMainModeIdentifier() {
        this.modeHierarchy.add("transit_walk");
        this.modeHierarchy.add("walk");
        this.modeHierarchy.add("bike");
        this.modeHierarchy.add("bicycle");
        this.modeHierarchy.add("ride");
        this.modeHierarchy.add("car");

        this.modeHierarchy.addAll(this.drtModes);

        this.modeHierarchy.add("pt");
        this.modeHierarchy.add("freight");
    }

    public String identifyMainMode(List<? extends PlanElement> planElements) {
        int mainModeIndex = -1 ;
        List<String> modesFound = new ArrayList<>();
        for ( PlanElement pe : planElements ) {
            int index;
            String mode;
            if ( pe instanceof Leg ) {
                Leg leg = (Leg) pe ;
                mode = leg.getMode();
            } else {
                continue;
            }
            if (mode.equals(TransportMode.non_network_walk)) {
                // skip, this is only a helper mode for access, egress and pt transfers
                continue;
            }
            if (mode.equals(TransportMode.transit_walk)) {
                mode = TransportMode.walk;
            } else {
                for (String drtMode: drtModes) {
                    if (mode.equals(drtMode + "_fallback")) {// transit_walk / drt_walk / ... to be replaced by _fallback soon
                        mode = TransportMode.walk;
                    }
                }
            }
            modesFound.add(mode);
            index = modeHierarchy.indexOf( mode ) ;
            if ( index < 0 ) {
                throw new RuntimeException("unknown mode=" + mode ) ;
            }
            if ( index > mainModeIndex ) {
                mainModeIndex = index ;
            }
        }
        if (mainModeIndex == -1) {
            throw new RuntimeException("no main mode found for trip " + planElements) ;
        }

        String mainMode = modeHierarchy.get( mainModeIndex ) ;
        // differentiate pt monomodal/intermodal
        if (mainMode.equals(TransportMode.pt)) {
            boolean isDrtPt = false;
            for (String modeFound: modesFound) {
                if (modeFound.equals(TransportMode.pt)) {
                    continue;
                } else if (modeFound.equals(TransportMode.walk)) {
                    continue;
                } else if (drtModes.contains(modeFound)) {
                    isDrtPt = true;
                } else {
                    log.error("unknown intermodal pt trip: " + planElements);
                    throw new RuntimeException("unknown intermodal pt trip");
                }
            }

            return TransportMode.pt;

        } else {
            return mainMode;
        }
    }
}
