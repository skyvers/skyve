package modules.admin.Snapshot;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

import modules.admin.domain.Snapshot;

@SuppressWarnings({ "static-method", "java:S1130" })
class SnapshotBizletTest {

	private static final SnapshotBizlet bizlet = new SnapshotBizlet();

	@Test
	void getVariantDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		assertNull(bizlet.getVariantDomainValues("unknownAttribute"));
	}

	@Test
	void getDynamicDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		assertNull(bizlet.getDynamicDomainValues("unknownAttribute", new Snapshot()));
	}

	@Test
	void preRerenderWithModuleNameSourceClearsQueryName() throws Exception {
		Snapshot bean = new Snapshot();
		bean.setQueryName("someQuery");
		bizlet.preRerender(Snapshot.moduleNamePropertyName, bean, null);
		assertNull(bean.getQueryName());
	}

	@Test
	void preRerenderWithUnknownSourceDoesNothing() throws Exception {
		Snapshot bean = Assertions.assertDoesNotThrow(Snapshot::new);
		bean.setQueryName("someQuery");
		bizlet.preRerender("unknownSource", bean, null);
		// query name unchanged, no exception
	}

	@Test
	void preSaveWithAlreadySetOrdinalDoesNothing() throws Exception {
		Snapshot bean = new Snapshot();
		bean.setOrdinal(Integer.valueOf(5));
		// not persisted, but ordinal is already set → no CORE call
		bizlet.preSave(bean);
		assertEquals(Integer.valueOf(5), bean.getOrdinal());
	}

	@Test
	void preSaveWithNullModuleNameSetsOrdinalToZero() throws Exception {
		Snapshot bean = new Snapshot();
		// moduleName is null, so ordinal is set to 0 without querying CORE
		bizlet.preSave(bean);
		assertEquals(Integer.valueOf(0), bean.getOrdinal());
	}

	@Test
	void preSaveWithNullQueryNameSetsOrdinalToZero() throws Exception {
		Snapshot bean = new Snapshot();
		bean.setModuleName("admin");
		// queryName is null, so ordinal is set to 0 without querying CORE
		bizlet.preSave(bean);
		assertEquals(Integer.valueOf(0), bean.getOrdinal());
	}
}
