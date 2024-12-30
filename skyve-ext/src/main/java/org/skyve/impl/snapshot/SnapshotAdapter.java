package org.skyve.impl.snapshot;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.annotation.Nullable;

/**
 * Class Used to convert between client snapshot payload definitions and Snapshot objects.
 * This class can be used as a validator or converter of different client snapshot payload types.
 */
public abstract class SnapshotAdapter {

    protected final Logger LOGGER = LoggerFactory.getLogger(getClass());

	/**
	 * SmartClient adapter.
	 */
	public static final SnapshotAdapter SMART_CLIENT = new SmartClientSnapshotAdapter();
	
	/**
	 * Vue adapter.
	 */
	public static final SnapshotAdapter VUE = new VueSnapshotAdapter();
	
	/**
	 * Detect of this is a SmartClient snapshot payload.
	 * @param snapshot	The potential SmartClient snapshot payload
	 * @return	true if SmartClient, otherwise false.
	 */
	private static boolean isSmartClientSnapshot(String snapshot) {
		return snapshot.contains("\"advancedCriteriaStyle\"");
	}
	
	/**
	 * Validate and convert a snapshot payload to a Vue snapshot payload.
	 * 
	 * @param snapshot	The snapshot payload to convert.
	 * @return	The corresponding Vue snapshot payload.
	 */
	public static @Nullable String toVue(String snapshot) {
		String result = null;
		
		if (isSmartClientSnapshot(snapshot)) {
			Snapshot snap = SMART_CLIENT.fromClientPayload(snapshot);
			if (snap != null) { // valid
				snap.getColumns().clear(); // remove visibility and width settings when converting
				result = VUE.toClientPayload(snap);
			}
		}
		else {
			if (VUE.fromClientPayload(snapshot) != null) {
				result = snapshot;
			}
		}

		return result;
	}
	
	/**
	 * Validate and convert a snapshot payload to a SmartClient snapshot payload.
	 * 
	 * @param snapshot	The snapshot payload to convert.
	 * @return	The corresponding SmartClient snapshot payload.
	 */
	public static @Nullable String toSmartClient(String snapshot) {
		String result = null;
		
		if (isSmartClientSnapshot(snapshot)) {
			if (SMART_CLIENT.fromClientPayload(snapshot) != null) {
				result = snapshot;
			}
		}
		else {
			Snapshot snap = VUE.fromClientPayload(snapshot);
			if (snap != null) { // valid
				snap.getColumns().clear(); // remove visibility and width settings when converting
				result = SMART_CLIENT.toClientPayload(snap);
			}
		}

		return result;
	}
	
	/**
	 * Take a String snapshot definition payload from a client and convert into a Snapshot object.
	 * 
	 * @param payload	The payload to convert.
	 * @return	A Snapshot representing the payload or null if conversion cannot occur.
	 */
	public abstract @Nullable Snapshot fromClientPayload(String payload);

	/**
	 * Take a Snapshot object and convert into a String snapshot definition payload for a client
	 * @param snapshot	The Snapshot to convert
	 * @return	The payload representing the Snapshot of null if conversion cannot occur.
	 */
	public abstract @Nullable String toClientPayload(Snapshot snapshot);
}
