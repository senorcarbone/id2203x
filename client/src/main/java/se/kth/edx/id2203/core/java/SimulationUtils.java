package se.kth.edx.id2203.core.java;


import se.sics.kompics.network.Msg;
import se.sics.kompics.simulator.network.NetworkModel;
import se.sics.kompics.simulator.network.identifier.Identifier;
import se.sics.kompics.simulator.network.identifier.IdentifierExtractor;
import se.sics.kompics.simulator.network.impl.DisconnectedNodesNetworkModel;

import java.util.Set;

public class SimulationUtils {
	
	
	public static NetworkModel newDisconnectedModel(IdentifierExtractor extr, NetworkModel within, 
													NetworkModel across, Set<Identifier> partition) {
		return new NetworkModel() {
			@Override
			public long getLatencyMs(Msg message) {

				Identifier srcId = extr.extract(message.getHeader().getSource());
				Identifier dstId = extr.extract(message.getHeader().getDestination());

				int srcPartition =  partition.contains(srcId) ? 0 : 1;
				int destPartition = partition.contains(dstId) ? 0 : 1;
				
				if (srcPartition == destPartition)
					return within.getLatencyMs(message);
				
				return across.getLatencyMs(message);
			}
		};
	}
}
