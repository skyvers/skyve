package modules.admin.DataGroup;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import modules.admin.domain.DataGroup;

@SuppressWarnings("static-method")
class DataGroupServiceTest {
	private static DataGroup dataGroup(String bizId, String name) {
		DataGroup result = new DataGroup();
		result.setBizId(bizId);
		result.setName(name);
		return result;
	}

	private static void injectPersistence(DataGroupService service, Persistence persistence) throws Exception {
		Field field = DataGroupService.class.getDeclaredField("pers");
		field.setAccessible(true);
		field.set(service, persistence);
	}

	@Test
	void getDataGroupDomainValuesUsesSuppliedPersistenceAndOrdersByName() throws Exception {
		Persistence persistence = mock(Persistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		when(persistence.newDocumentQuery(DataGroup.MODULE_NAME, DataGroup.DOCUMENT_NAME)).thenReturn(query);
		when(query.beanResults()).thenReturn(List.of(dataGroup("dg-1", "Alpha"), dataGroup("dg-2", "Beta")));

		List<DomainValue> values = new DataGroupService().getDataGroupDomainValues(persistence);

		verify(query).addBoundOrdering(DataGroup.namePropertyName, SortDirection.ascending);
		assertEquals(2, values.size());
		assertThat(values.get(0).getCode(), is("dg-1"));
		assertThat(values.get(1).getCode(), is("dg-2"));
	}

	@Test
	void getDataGroupDomainValuesFallsBackToInjectedPersistence() throws Exception {
		DataGroupService service = new DataGroupService();
		Persistence persistence = mock(Persistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		injectPersistence(service, persistence);
		when(persistence.newDocumentQuery(DataGroup.MODULE_NAME, DataGroup.DOCUMENT_NAME)).thenReturn(query);
		when(query.beanResults()).thenReturn(List.of());

		List<DomainValue> values = service.getDataGroupDomainValues(null);

		verify(persistence).newDocumentQuery(DataGroup.MODULE_NAME, DataGroup.DOCUMENT_NAME);
		assertEquals(0, values.size());
	}

	@Test
	void getBeanDataGroupReturnsNullWhenBeanHasNoDataGroup() throws Exception {
		DataGroupService service = new DataGroupService();
		Persistence persistence = mock(Persistence.class);
		Bean bean = mock(Bean.class);
		injectPersistence(service, persistence);

		DataGroup result = service.getBeanDataGroup(bean);

		assertThat(result, is((DataGroup) null));
		verifyNoInteractions(persistence);
	}

	@Test
	void getBeanDataGroupRetrievesDataGroupByBizDataGroupId() throws Exception {
		DataGroupService service = new DataGroupService();
		Persistence persistence = mock(Persistence.class);
		Bean bean = mock(Bean.class);
		DataGroup dataGroup = dataGroup("dg-1", "Alpha");
		injectPersistence(service, persistence);
		when(bean.getBizDataGroupId()).thenReturn("dg-1");
		when(persistence.retrieve(DataGroup.MODULE_NAME, DataGroup.DOCUMENT_NAME, "dg-1")).thenReturn(dataGroup);

		DataGroup result = service.getBeanDataGroup(bean);

		assertThat(result, is(dataGroup));
	}
}
