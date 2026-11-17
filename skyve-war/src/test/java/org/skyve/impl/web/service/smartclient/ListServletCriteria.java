package org.skyve.impl.web.service.smartclient;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.snapshot.CompoundFilterOperator;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.InMemoryFilter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.util.Binder;

/**
 * The list servlet H2 tests' criteria, as a SmartClient list grid sends them, and the two ways they filter rows: a
 * document query in the database, and an in-memory filter over beans. Rows are named by a text binding.
 */
final class ListServletCriteria {
	private ListServletCriteria() {
		// static methods only
	}

	/**
	 * A criterion, its value a string as in the request's JSON, or a list (for inSet) when there is no value.
	 */
	static Map<String, Object> criterion(String fieldName, String operator, String value, String... values) {
		Map<String, Object> result = new HashMap<>();
		result.put("fieldName", fieldName);
		result.put("operator", operator);
		if (value != null) {
			result.put("value", value);
		}
		else if (values.length > 0) {
			result.put("value", new ArrayList<>(List.of(values)));
		}
		return result;
	}

	/** A range criterion, either end missing when null. */
	static Map<String, Object> range(String fieldName, String operator, String start, String end) {
		Map<String, Object> result = criterion(fieldName, operator, null);
		if (start != null) {
			result.put("start", start);
		}
		if (end != null) {
			result.put("end", end);
		}
		return result;
	}

	@SafeVarargs
	static Map<String, Object> group(String operator, Map<String, Object>... criteria) {
		return Map.of("operator", operator, "criteria", List.of(criteria));
	}

	/** The names of the rows a query's list model fetches from the database with the criteria. */
	static Set<String> fetch(Module module,
								Document document,
								String queryName,
								String nameBinding,
								CompoundFilterOperator operator,
								List<Map<String, Object>> criteria)
	throws Exception {
		Customer customer = CORE.getCustomer();
		// a null query name is the document's default query
		MetaDataQueryDefinition query = (queryName == null) ?
											module.getDocumentDefaultQuery(customer, document.getName()) :
											module.getNullSafeMetaDataQuery(queryName);
		DocumentQueryListModel<Bean> model = new DocumentQueryListModel<>(query);
		model.postConstruct(customer, true);
		model.setStartRow(0);
		model.setEndRow(100);
		SmartClientListServlet.addAdvancedFilterCriteriaToQuery(module, document, CORE.getUser(), operator, criteria, null, model);
		Page page = model.fetch();
		return names(Objects.requireNonNull(page.getRows()), nameBinding);
	}

	/** The names of the rows an in-memory filter keeps with the criteria. */
	static Set<String> filterInMemory(Module module,
										Document document,
										List<Bean> rows,
										String nameBinding,
										CompoundFilterOperator operator,
										List<Map<String, Object>> criteria)
	throws Exception {
		InMemoryFilter filter = new InMemoryFilter();
		ListModel<Bean> model = mock(ListModel.class);
		when(model.getFilter()).thenReturn(filter);
		when(model.newFilter()).thenAnswer(invocation -> new InMemoryFilter());
		SmartClientListServlet.addAdvancedFilterCriteriaToQuery(module, document, CORE.getUser(), operator, criteria, null, model);
		List<Bean> filtered = new ArrayList<>(rows);
		filter.filter(filtered);
		return names(filtered, nameBinding);
	}

	static Set<String> names(List<Bean> beans, String nameBinding) {
		Set<String> result = new TreeSet<>();
		for (Bean bean : beans) {
			result.add((String) Binder.get(bean, nameBinding));
		}
		return result;
	}
}
