package org.matsim.analysis;


import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.events.LinkEnterEvent;
import org.matsim.api.core.v01.events.VehicleEntersTrafficEvent;
import org.matsim.api.core.v01.events.handler.LinkEnterEventHandler;
import org.matsim.api.core.v01.events.handler.VehicleEntersTrafficEventHandler;
import org.matsim.api.core.v01.network.Link;
import org.matsim.api.core.v01.network.Network;
import org.matsim.application.MATSimAppCommand;
import org.matsim.core.api.experimental.events.EventsManager;
import org.matsim.core.events.EventsUtils;
import org.matsim.core.events.MatsimEventsReader;
import org.matsim.core.network.NetworkUtils;
import picocli.CommandLine;

import java.io.FileWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

import static org.matsim.application.ApplicationUtils.globFile;

/**
 * @author Simon Meinhardt (simei94)
 */

public class VehiclesRoadUsageAnalysis implements MATSimAppCommand {
    @CommandLine.Option(names = "--directory", description = "path to the directory of the simulation output", required = true)
    private Path directory;

    public static void main(String[] args) {
        new VehiclesRoadUsageAnalysis().execute(args);
    }

    static class VehicleLinkUsageCounter implements VehicleEntersTrafficEventHandler, LinkEnterEventHandler {

        private final Network network;
        private final Map<Id<Link>, Integer> vehicleCount;

        VehicleLinkUsageCounter(Network network, Map<Id<Link>, Integer> vehicleCount) {
            this.network = network;
            this.vehicleCount = vehicleCount;
            reset(0);
        }

        @Override
        public void handleEvent(LinkEnterEvent event) {
            if(!vehicleCount.containsKey(event.getLinkId())) {
                vehicleCount.put(event.getLinkId(), 1);
            } else {
                vehicleCount.replace(event.getLinkId(), vehicleCount.get(event.getLinkId()) + 1);
            }
        }

        @Override
        public void handleEvent(VehicleEntersTrafficEvent event) {
            if(!vehicleCount.containsKey(event.getLinkId())) {
                vehicleCount.put(event.getLinkId(), 1);
            } else {
                vehicleCount.replace(event.getLinkId(), vehicleCount.get(event.getLinkId()) + 1);
            }
        }

        @Override
        public void reset(int iteration) {
            vehicleCount.clear();
        }
    }

    @Override
    public Integer call() throws Exception {
        Path networkPath = globFile(directory, "*output_network.*");
        Path eventsFilePath = globFile(directory, "*output_events.*");
        Path outputFolder = Path.of(directory.toString() + "/analysis-road-usage");

        if (!Files.exists(outputFolder)) {
            Files.createDirectory(outputFolder);
        }

        Network network = NetworkUtils.readNetwork(networkPath.toString());
        EventsManager eventsManager = EventsUtils.createEventsManager();

        Map<Id<Link>, Integer> vehicleCount = new HashMap<>();

        VehicleLinkUsageCounter vehicleLinkUsageCounter = new VehicleLinkUsageCounter(network, vehicleCount);
        eventsManager.addHandler(vehicleLinkUsageCounter);

        MatsimEventsReader reader = new MatsimEventsReader(eventsManager);
        reader.readFile(eventsFilePath.toString());

        //writeResults
        String vehicleRoadUsageFile = outputFolder + "/" + "allModes_vehicle_road_usage.tsv";
        CSVPrinter vehicleRoadUsageWriter = new CSVPrinter(new FileWriter(vehicleRoadUsageFile), CSVFormat.TDF);
        List<String> header = new ArrayList<>();
        header.add("link_id");
        header.add("vehicleCount");

        vehicleRoadUsageWriter.printRecord(header);

        for(Id<Link> linkId : vehicleCount.keySet()) {
            List<String> vehicleEntry = new ArrayList<>();
            vehicleEntry.add(linkId.toString());
            vehicleEntry.add(vehicleCount.get(linkId).toString());
            vehicleRoadUsageWriter.printRecord(vehicleEntry);
        }
        vehicleRoadUsageWriter.close();
        return 0;
    }
}
