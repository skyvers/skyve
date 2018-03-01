package org.skyve.impl.tools.test.sail.interpret;

import java.io.FileReader;

import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.execution.PrimeFacesInlineSeleneseExecutor;
import org.skyve.impl.tools.test.sail.language.TestSuite;

public class Interpreter {
	protected static final String USER = "TestUser";
	protected static final String CUSTOMER = "bizhub";

	public static void main(String[] args) throws Exception {
//		String xml = Generator.visitModules("bizhub");
//System.out.println(xml);
//		TestSuite testSuite = XMLUtil.unmarshalSAIL(new StringReader(xml));
		AbstractRepository.set(new LocalDesignRepository());

		final SuperUser user = new SuperUser();
		user.setCustomerName(CUSTOMER);
		user.setName(USER);
		user.setId(USER);

		try (FileReader fr = new FileReader("/Users/mike/dtf/skyve/skyve-tools/test.xml")) {
			TestSuite testSuite = XMLUtil.unmarshalSAIL(fr);
			PrimeFacesInlineSeleneseExecutor executor = new PrimeFacesInlineSeleneseExecutor(user);
			testSuite.execute(executor);
			System.out.println(executor);
		}
	}
}
