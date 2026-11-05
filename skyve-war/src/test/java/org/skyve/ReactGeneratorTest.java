package org.skyve;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.client.react.ReactGenerator;

import util.AbstractH2Test;

@Disabled("fix this")
class ReactGeneratorTest extends AbstractH2Test {
	@Test
	@SuppressWarnings("static-method")
	void generate() {
		Assertions.assertDoesNotThrow(() -> ReactGenerator.main(new String[0]));
	}
}
