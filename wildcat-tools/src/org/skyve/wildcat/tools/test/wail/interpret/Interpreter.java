package org.skyve.wildcat.tools.test.wail.interpret;

import java.io.StringReader;

import org.skyve.wildcat.tools.test.wail.XMLUtil;
import org.skyve.wildcat.tools.test.wail.generate.Generator;
import org.skyve.wildcat.tools.test.wail.language.TestSuite;

public class Interpreter {
	public static void main(String[] args) throws Exception {
		String xml = Generator.visitModules("bizhub");
System.out.println(xml);
		TestSuite testSuite = XMLUtil.unmarshalWAIL(new StringReader(xml));
		StringBuilder script = new StringBuilder(2048);
		testSuite.execute(script);
		System.out.println(script);
	}
}
