package org.skyve.impl.snapshot;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.snapshot.Snapshot.AdvancedSearchType;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.JSON;
import org.skyve.util.OWASP;

class SmartClientSnapshotAdapter extends SnapshotAdapter {
	private static final String SC_ADVANCED_CRITERIA_STYLE = "advancedCriteriaStyle";
	private static final String SC_FIELD_STATE = "fieldState";
	private static final String SC_NAME = "name"; 
	private static final String SC_WIDTH = "width";
	private static final String SC_SORT_STATE = "sortState";
	private static final String SC_PROPERTY = "property";
	private static final String SC_DIRECTION = "direction";
	private static final String SC_DESCENDING = "descending";
	private static final String SC_GROUP_STATE = "groupState";
	private static final String SC_FIELD_NAME = "fieldName";
	private static final String SC_SUMMARY_TYPE = "summaryType";
	private static final String SC_CRITERIA = "criteria";
	private static final String SC_CONSTRUCTOR = "_constructor";
	private static final String SC_ADVANCED_CRITERIA = "AdvancedCriteria";
	private static final String SC_OPERATOR = "operator";
	private static final String SC_VALUE = "value";
	private static final String SC_START = "start";
	private static final String SC_END = "end";
	
	@Override
	public Snapshot fromClientPayload(String payload) {
		User u = CORE.getUser();
		
		Snapshot result = null;
		try {
			@SuppressWarnings("unchecked")
			Map<String, Object> sc = (Map<String, Object>) JSON.unmarshall(u, payload);
			result = new Snapshot();
			String advancedCriteriaStyle = (String) sc.get(SC_ADVANCED_CRITERIA_STYLE);
			result.setAdvanced((advancedCriteriaStyle == null) ? null : AdvancedSearchType.valueOf(advancedCriteriaStyle));

			// FieldState is stringified JSON
			Object value = sc.get(SC_FIELD_STATE);
			if (value instanceof String) {
				String fieldState = (String) value;
				value = JSON.unmarshall(u, fieldState);
				if (value instanceof List) {
					@SuppressWarnings("unchecked")
					List<Object> list = (List<Object>) value;
					for (Object element : list) {
						if (element instanceof String) {
							String column = (String) element;
							if (! (PersistentBean.TAGGED_NAME.equals(column) || PersistentBean.FLAG_COMMENT_NAME.equals(column))) {
								result.putColumn(column);
							}
						}
						else if (element instanceof Map) {
							@SuppressWarnings("unchecked")
							Map<String, Object> field = (Map<String, Object>) element;
							Object column = field.get(SC_NAME);
							if (column instanceof String) {
								if (! (PersistentBean.TAGGED_NAME.equals(column) || PersistentBean.FLAG_COMMENT_NAME.equals(column))) {
									Object width = field.get(SC_WIDTH);
									if (width instanceof Number) {
										result.putColumn((String) column, ((Number) width).intValue());
									}
									else {
										result.putColumn((String) column);
									}
								}
							}
							else {
								throw new IllegalStateException("Malformed fieldState column in snapshot - " + column);
							}
						}
						else {
							throw new IllegalStateException("Malformed fieldState in snapshot - " + element);
						}
					}
				}
				else {
					throw new IllegalStateException("Malformed fieldState in snapshot - " + value);
				}
			}
			else {
				throw new IllegalStateException("Malformed fieldState in snapshot - " + value);
			}
			
			// SortState can be null if empty, or an object
			value = sc.get(SC_SORT_STATE);
			if (value instanceof String) {
				// Sort state has round brackets in it which sux - so extract only the sortSpecifiers from that mess
				String sortState = UtilImpl.processStringValue((String) value);
				if (sortState != null) {
					int startIndex = sortState.indexOf("sortSpecifiers:");
					if (startIndex >= 0) {
						startIndex += 15;
	
						int endIndex = sortState.indexOf("]}");
						if (endIndex >= 0) {
							endIndex += 1;
	
							sortState = sortState.substring(startIndex, endIndex);
							value = JSON.unmarshall(u, sortState);
							if (value instanceof List) {
								@SuppressWarnings("unchecked")
								List<Map<String, Object>> list = (List<Map<String, Object>>) value;
								for (Map<String, Object> sort : list) {
									Object property = sort.get(SC_PROPERTY);
									if (property instanceof String) {
										Object direction = sort.get(SC_DIRECTION);
										if (SC_DESCENDING.equals(direction)) {
											result.putSort((String) property, SortDirection.descending);
										}
										else {
											result.putSort((String) property);
										}
									}
									else {
										throw new IllegalStateException("Malformed sortState property in snapshot - " + property);
									}
								}
							}
							else {
								throw new IllegalStateException("Malformed sortState in snapshot - " + value);
							}
						}
						else {
							throw new IllegalStateException("Malformed sortState in snapshot - " + sortState);
						}
					}
					else {
						throw new IllegalStateException("Malformed sortState in snapshot - " + sortState);
					}
				}
			}
			else if (value != null) {
				throw new IllegalStateException("Malformed sortState in snapshot - " + value);
			}

			
			// GroupState can be null if empty, or an object
			value = sc.get(SC_GROUP_STATE);
			if (value instanceof String) {
				// Group state has round brackets in it which sux - so remove all opening and closing round brackets
				String groupState = UtilImpl.processStringValue((String) value);
				if (groupState != null) {
					groupState = groupState.replace('(', ' ').replace(')', ' ');
					value = JSON.unmarshall(u, groupState);
					if (value instanceof List) {
						@SuppressWarnings("unchecked")
						List<Map<String, Object>> list = (List<Map<String, Object>>) value;
						for (Map<String, Object> sort : list) {
							Object fieldName = sort.get(SC_FIELD_NAME);
							if (fieldName instanceof String) {
								result.setGroup((String) fieldName);
							}
							else {
								throw new IllegalStateException("Malformed groupState fieldName in snapshot - " + fieldName);
							}
						}
					}
					else {
						throw new IllegalStateException("Malformed groupState in snapshot - " + value);
					}
				}
			}
			else {
				throw new IllegalStateException("Malformed groupState in snapshot - " + value);
			}
			
			// SummaryType can be null or an empty String if empty, or a String depicting the summary type
			value = sc.get(SC_SUMMARY_TYPE);
			if (value instanceof String) {
				String summaryType = UtilImpl.processStringValue((String) value);
				if (summaryType != null) {
					result.setSummary(AggregateFunction.valueOf(summaryType));
				}
			}
			else {
				throw new IllegalStateException("Malformed summaryType in snapshot - " + value);
			}

			// Criteria is null or a Map of Maps of Simple (Criterion) and Advanced (Criteria)
			value = sc.get(SC_CRITERIA);
			if (value instanceof Map) {
				@SuppressWarnings("unchecked")
				Map<String, Object> criteria = (Map<String, Object>) value;
				result.setSourceSmartClientCriteria(criteria);
				if (! criteria.isEmpty()) {
					result.setFilter(criteria(criteria));
				}
			}
			else {
				throw new IllegalStateException("Malformed criteria in snapshot - " + value);
			}
		}
		catch (Exception e) {
			UtilImpl.LOGGER.warning("Snapshot could not be created from SmartClient Payload " + payload);
			e.printStackTrace();
			result = null;
		}
		
		return result;
	}

	private static SnapshotFilter criteria(Map<String, Object> map) {
		// Advanced Criteria
		if (SC_ADVANCED_CRITERIA.equals(map.get(SC_CONSTRUCTOR))) {
			SnapshotCriteria result = new SnapshotCriteria();
			String operator = (String) map.get(SC_OPERATOR);
			result.setOperator(CompoundFilterOperator.valueOf(operator));
			
			List<SnapshotFilter> filters = result.getFilters();
			@SuppressWarnings("unchecked")
			List<Map<String, Object>> list = (List<Map<String, Object>>) map.get(SC_CRITERIA);
			for (Map<String, Object> element : list) {
				if (! element.isEmpty()) {
					filters.add(criteria(element));
				}
			}
			return result;
		}
		
		// Simple operator criteria
		Object fieldName = map.get(SC_FIELD_NAME);
		Object operator = map.get(SC_OPERATOR);
		if ((fieldName instanceof String) && (operator instanceof String)) {
			SnapshotCriterion result = new SnapshotCriterion();
			result.setColumn((String) map.get(SC_FIELD_NAME));
			result.setOperator(SmartClientFilterOperator.valueOf((String) map.get(SC_OPERATOR)));
			result.setValue(map.get(SC_VALUE));
			result.setStart(map.get(SC_START));
			result.setEnd(map.get(SC_END));
			return result;
		}
		
		// Simple map criteria
		SnapshotCriteria result = new SnapshotCriteria();
		List<SnapshotFilter> filters = result.getFilters();
		for (Entry<String, Object> entry : map.entrySet()) {
			String key = entry.getKey();
			Object value = entry.getValue();
			// List value - process depending on the size of the list
			if (value instanceof List) {
				@SuppressWarnings("unchecked")
				List<Object> list = (List<Object>) value;
				int size = list.size();
				// empty list - continue
				if (size == 0) {
					continue;
				}
				// Singleton list - add simple criteria
				else if (size == 1) {
					SnapshotCriterion child = new SnapshotCriterion();
					child.setColumn(key);
					child.setOperator(SmartClientFilterOperator.equals);
					child.setValue(value);
					filters.add(child);
				}
				// Multiple - add or'd criteria
				else {
					SnapshotCriteria or = new SnapshotCriteria();
					or.setOperator(CompoundFilterOperator.or);
					List<SnapshotFilter> ored = or.getFilters();
					for (Object element : list) {
						SnapshotCriterion child = new SnapshotCriterion();
						child.setColumn(key);
						child.setOperator(SmartClientFilterOperator.equals);
						child.setValue(element);
						ored.add(child);
					}
					filters.add(or);
				}
			}
			// Simple value - add a criterion
			else {
				SnapshotCriterion child = new SnapshotCriterion();
				child.setColumn(key);
				child.setOperator(SmartClientFilterOperator.iContains);
				child.setValue(value);
				filters.add(child);
			}
		}
		
		return result;
	}
	
	@Override
	public String toClientPayload(Snapshot snapshot) {
		StringBuilder result = new StringBuilder(512);

		// Advanced search
		AdvancedSearchType advanced = snapshot.getAdvanced();
		result.append("{\"").append(SC_ADVANCED_CRITERIA_STYLE).append("\":");
		if (advanced == null) {
			result.append("null,");
		}
		else {
			result.append("\"").append(advanced).append("\",");
		}

		// Field state
		result.append('"').append(SC_FIELD_STATE).append("\":\"[\\\"");
		result.append(PersistentBean.TAGGED_NAME).append("\\\",\\\"");
		result.append(PersistentBean.FLAG_COMMENT_NAME).append("\\\",");
		
		for (Entry<String, Integer> column : snapshot.getColumns().entrySet()) {
			String key = column.getKey();
			result.append('{').append(SC_NAME).append(":\\\"").append(OWASP.escapeJsString(key));
			Integer value = column.getValue();
			if (value == null) {
				result.append("\\\",autoFitWidth:false},");
			}
			else {
				result.append("\\\",width:").append(value).append("},");
			}
		}
		result.setLength(result.length() - 1); // remove last comma
		result.append("]\",");
		
		// Sort state
		result.append('"').append(SC_SORT_STATE).append("\":");
		Map<String, SortDirection> sorts = snapshot.getSorts();
		if (sorts.isEmpty()) {
			result.append("null,");
		}
		else {
			result.append("\"({sortSpecifiers:[");
			
			sorts.forEach((c, d) -> {
				result.append('{').append(SC_PROPERTY).append(":\\\"").append(OWASP.escapeJsString(c));
				result.append("\\\",").append(SC_DIRECTION).append(":\\\"").append(d).append("\\\"},");
			});
			result.setLength(result.length() - 1); // remove last comma
			result.append("]})\",");
		}

		// Group State ("" if empty)
		result.append('"').append(SC_GROUP_STATE).append("\":\"");
		String group = snapshot.getGroup();
		if (group != null) {
			result.append("([{").append(SC_FIELD_NAME).append(":\"").append(OWASP.escapeJsString(group)).append("\"}])");
		}
		result.append("\",");

		// Summary Type ("" if empty)
		result.append('"').append(SC_SUMMARY_TYPE).append("\":\"");
		AggregateFunction summary = snapshot.getSummary();
		if (summary != null) {
			result.append(summary);
		}
		result.append("\",");
		
		// Criteria
		result.append('"').append(SC_CRITERIA).append("\":");
		SnapshotFilter filter = snapshot.getFilter();
		if (filter == null) {
			result.append("{}");
		}
		else {
			result.append(JSON.marshall(criteria(filter)));
		}
		result.append('}');
		
		return result.toString();
	}
	
	private static Map<String, Object> criteria(SnapshotFilter criteria) {
		Map<String, Object> result = new LinkedHashMap<>();

		// Advanced Criteria
		if (criteria instanceof SnapshotCriteria) {
			SnapshotCriteria advanced = (SnapshotCriteria) criteria;
			result.put(SC_CONSTRUCTOR, SC_ADVANCED_CRITERIA);
			result.put(SC_OPERATOR, advanced.getOperator());
			
			List<SnapshotFilter> filters = advanced.getFilters();
			List<Map<String, Object>> array = new ArrayList<>(filters.size());
			for (SnapshotFilter child : filters) {
				array.add(criteria(child)); 
			}
			result.put(SC_CRITERIA, array);
		}
		// Simple Criteria
		else {
			SnapshotCriterion simple = (SnapshotCriterion) criteria;
			result.put(SC_FIELD_NAME, simple.getColumn());
			result.put(SC_OPERATOR, simple.getOperator());
			Object value = simple.getValue();
			if (value != null) {
				result.put(SC_VALUE, value);
			}
			else {
				result.put(SC_START, simple.getStart());
				result.put(SC_END, simple.getEnd());
			}
		}

		return result;
	}
}
