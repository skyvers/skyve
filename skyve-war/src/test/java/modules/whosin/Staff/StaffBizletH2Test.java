package modules.whosin.Staff;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ImplicitActionName;

import modules.whosin.domain.Staff;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class StaffBizletH2Test extends AbstractH2Test {

	private static final StaffBizlet bizlet = new StaffBizlet();

	private Staff bean;
	private MockWebContext webContext;

	@BeforeEach
	void setUpBean() {
		bean = new StaffExtension();
		webContext = new MockWebContext();
	}

	@Test
	void preExecuteNewReturnsBean() throws Exception {
		Staff result = bizlet.preExecute(ImplicitActionName.New, bean, null, webContext);
		assertNotNull(result);
	}

	@Test
	void preExecuteSaveWithNoOriginalValuesReturnsBean() throws Exception {
		// No originalValues changes - the reportTo branch is skipped
		Staff result = bizlet.preExecute(ImplicitActionName.Save, bean, null, webContext);
		assertNotNull(result);
	}

	@Test
	void preExecuteOKWithNoOriginalValuesReturnsBean() throws Exception {
		Staff result = bizlet.preExecute(ImplicitActionName.OK, bean, null, webContext);
		assertNotNull(result);
	}

	@Test
	void preRerenderWithStatusSourceAndNullOfficeDoesNotThrow() throws Exception {
		bean.setStatus(Staff.Status.inTheOffice);
		bean.setBaseOffice(null);
		// Should not throw - no office means skips the setLocation branch
		bizlet.preRerender(Staff.statusPropertyName, bean, webContext);
	}

	@Test
	void preRerenderWithOtherSourceDoesNotThrow() throws Exception {
		bizlet.preRerender(Staff.staffCodePropertyName, bean, webContext);
	}
}
