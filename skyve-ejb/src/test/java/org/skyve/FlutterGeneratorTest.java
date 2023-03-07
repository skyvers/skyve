package org.skyve;

import org.junit.Ignore;
import org.junit.Test;
import org.skyve.impl.generate.client.flutter.FlutterGenerator;

import util.AbstractH2Test;

@Ignore("fix this")
public class FlutterGeneratorTest extends AbstractH2Test {
	@Test
	@SuppressWarnings("static-method")
	public void generate() throws Exception {
		FlutterGenerator.main(new String[0]);
	}
}
