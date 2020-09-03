package org.skyve.impl.web.spring;

import org.skyve.util.Util;

public class SkyveConfig {
	@SuppressWarnings("static-method")
	public String getSkyveContextUrl() {
		return Util.getSkyveContextUrl();
	}
}
