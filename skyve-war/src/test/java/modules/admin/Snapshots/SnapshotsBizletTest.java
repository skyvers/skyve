package modules.admin.Snapshots;

import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import modules.admin.domain.Snapshots;

@SuppressWarnings("static-method")
public class SnapshotsBizletTest {

	private static final SnapshotsBizlet bizlet = new SnapshotsBizlet();

	@Test
	void getVariantDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		assertNull(bizlet.getVariantDomainValues("unknownAttribute"));
	}

	@Test
	void getDynamicDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		assertNull(bizlet.getDynamicDomainValues("unknownAttribute", new Snapshots()));
	}

	@Test
	void preRerenderWithModuleNameSourceClearsQueryName() throws Exception {
		Snapshots bean = new Snapshots();
		bean.setQueryName("someQuery");
		bizlet.preRerender(Snapshots.moduleNamePropertyName, bean, null);
		assertNull(bean.getQueryName());
	}

	@Test
	void preRerenderWithUnknownSourceDoesNothing() throws Exception {
		Snapshots bean = new Snapshots();
		bean.setQueryName("someQuery");
		bizlet.preRerender("unknownSource", bean, null);
		// no exception expected
	}
}
