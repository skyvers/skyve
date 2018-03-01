package org.skyve.impl.tools.test.sail.interpret;

import java.io.FileReader;
import java.io.StringReader;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.execution.PrimeFacesInlineSeleneseExecutor;
import org.skyve.impl.tools.test.sail.generate.Generator;
import org.skyve.impl.tools.test.sail.language.TestSuite;

public class Interpreter {
	public static void main(String[] args) throws Exception {
//		String xml = Generator.visitModules("bizhub");
//System.out.println(xml);
//		TestSuite testSuite = XMLUtil.unmarshalSAIL(new StringReader(xml));
		try (FileReader fr = new FileReader("/Users/mike/dtf/skyve/skyve-tools/test.xml")) {
			TestSuite testSuite = XMLUtil.unmarshalSAIL(fr);
			PrimeFacesInlineSeleneseExecutor executor = new PrimeFacesInlineSeleneseExecutor();
			testSuite.execute(executor);
			System.out.println(executor);
		}
	}
}
