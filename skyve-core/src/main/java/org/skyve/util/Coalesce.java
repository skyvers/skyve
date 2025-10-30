package org.skyve.util;

public class Coalesce {

	private Coalesce() {
		// Make the class non-instantiable
	}

	public static <T> T coalesceNull(T val, T ifNullValue) {
		return (val == null ? ifNullValue : val);
	}
}
