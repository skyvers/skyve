package modules.admin.Group;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import modules.admin.domain.Group;

class GroupServiceTest {
	private Persistence persistence;
	private DocumentQuery query;
	private DocumentFilter filter;

	@BeforeEach
	void setUp() {
		persistence = mock(Persistence.class);
		query = mock(DocumentQuery.class);
		filter = mock(DocumentFilter.class);
		when(persistence.newDocumentQuery(Group.MODULE_NAME, Group.DOCUMENT_NAME)).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
	}

	@Test
	void getRetrievesGroupByDocumentId() throws Exception {
		GroupService service = serviceWithPersistence();
		GroupExtension group = new GroupExtension();
		when(query.beanResult()).thenReturn(group);

		GroupExtension result = service.get("group-id");

		assertThat(result, is(group));
		verify(persistence).newDocumentQuery(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		verify(filter).addEquals(Bean.DOCUMENT_ID, "group-id");
	}

	@Test
	void getAllReturnsQueryResults() throws Exception {
		GroupService service = serviceWithPersistence();
		List<GroupExtension> groups = List.of(new GroupExtension());
		when(query.<GroupExtension>beanResults()).thenReturn(groups);

		List<GroupExtension> result = service.getAll();

		assertThat(result, is(groups));
		verify(persistence).newDocumentQuery(Group.MODULE_NAME, Group.DOCUMENT_NAME);
	}

	@Test
	void findByNameLimitsAndFiltersByGroupName() throws Exception {
		GroupService service = serviceWithPersistence();
		GroupExtension group = new GroupExtension();
		when(query.beanResult()).thenReturn(group);

		GroupExtension result = service.findByName("Administrators");

		assertThat(result, is(group));
		verify(filter).addEquals(Group.namePropertyName, "Administrators");
		verify(query).setMaxResults(1);
	}

	private GroupService serviceWithPersistence() throws Exception {
		GroupService service = new GroupService();
		Field field = GroupService.class.getDeclaredField("persistence");
		field.setAccessible(true);
		field.set(service, persistence);
		return service;
	}
}
