package org.skyve.metadata.controller;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * An InputStream that will only be closed by Skyve once its contents have been processed.
 * This stream is used in download and upload actions.
 * 
 * @author mike
 */
public class WebFileInputStream extends FilterInputStream {
	private volatile boolean processed = false;
	
	public WebFileInputStream(InputStream in) {
		super(in);
	}
	
	public void processed() {
		processed = true;
	}

	@Override
	public void close() throws IOException {
		if (processed) {
			super.close();
		}
	}
}
