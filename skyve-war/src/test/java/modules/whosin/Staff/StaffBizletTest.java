package modules.whosin.Staff;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ImplicitActionName;

import modules.whosin.domain.Staff;

@SuppressWarnings("static-method")
public class StaffBizletTest {

	@Test
	void preExecuteWithEditSetsReportsToParent() throws Exception {
		StaffBizlet bizlet = new StaffBizlet();
		Staff bean = new Staff();
		// getParent() returns null when bizParentId is null → no CORE call
		Staff result = bizlet.preExecute(ImplicitActionName.Edit, bean, null, null);
		assertNotNull(result);
		assertNull(result.getReportsTo()); // parent was null, so reportsTo set to null
	}

	@Test
	void preExecuteWithDeleteReturnsBean() throws Exception {
		StaffBizlet bizlet = new StaffBizlet();
		Staff bean = new Staff();
		Staff result = bizlet.preExecute(ImplicitActionName.Delete, bean, null, null);
		assertNotNull(result);
	}

	@Test
	void preRerenderWithUnknownSourceDoesNothing() throws Exception {
		StaffBizlet bizlet = new StaffBizlet();
		Staff bean = new Staff();
		// unknown source → condition on statusPropertyName is false, no-op
		bizlet.preRerender("unknownSource", bean, null);
		assertNull(bean.getStatus());
	}

	@Test
	void preRerenderWithStatusSourceButNotInOfficeDoesNotSetLocation() throws Exception {
		StaffBizlet bizlet = new StaffBizlet();
		Staff bean = new Staff();
		bean.setStatus(Staff.Status.outOfTheOffice);
		// status is not inTheOffice → no location set
		bizlet.preRerender(Staff.statusPropertyName, bean, null);
		assertNull(bean.getLocation());
	}
}
