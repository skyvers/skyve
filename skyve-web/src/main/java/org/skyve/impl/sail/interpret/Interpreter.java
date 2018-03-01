package org.skyve.impl.sail.interpret;

import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.sail.execution.PrimeFacesInlineSeleneseExecutor;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.language.TestSuite;

public class Interpreter {
	protected static final String USER = "TestUser";
	protected static final String CUSTOMER = "bizhub";

	public static void main(String[] args) throws Exception {
//generator should create Suite
//		String xml = Generator.visitModules("bizhub");
//System.out.println(xml);
//		Suite testSuite = XMLUtil.unmarshalSAIL(new StringReader(xml));
		AbstractRepository.set(new LocalDesignRepository());

		final SuperUser user = new SuperUser();
		user.setCustomerName(CUSTOMER);
		user.setName(USER);
		user.setId(USER);

		TestSuite testSuite = XMLMetaData.unmarshalSAIL("/Users/mike/dtf/skyve/skyve-tools/test.xml");
		PrimeFacesInlineSeleneseExecutor executor = new PrimeFacesInlineSeleneseExecutor(user);
		testSuite.execute(executor);
		System.out.println(executor);
	}
}
