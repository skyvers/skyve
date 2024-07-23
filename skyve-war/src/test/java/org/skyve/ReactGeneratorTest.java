package org.skyve;

import org.junit.Ignore;
import org.junit.Test;
import org.skyve.impl.generate.client.react.ReactGenerator;

import util.AbstractH2TestForJUnit4;

@Ignore("fix this")
public class ReactGeneratorTest extends AbstractH2TestForJUnit4 {
	@Test
	@SuppressWarnings("static-method")
	public void generate() throws Exception {
		ReactGenerator.main(new String[0]);
	}
}
