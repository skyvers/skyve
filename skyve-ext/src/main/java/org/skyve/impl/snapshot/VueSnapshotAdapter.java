package org.skyve.impl.snapshot;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.skyve.CORE;
import org.skyve.impl.snapshot.Snapshot.AdvancedSearchType;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.JSON;

import jakarta.annotation.Nullable;

class VueSnapshotAdapter extends SnapshotAdapter {
	private static final String VUE_FILTERS = "filters";
	private static final String VUE_VISIBLE_COLUMNS = "visibleColumns";
	private static final String VUE_COLUMN_WIDTHS = "columnWidths";
	private static final String VUE_SORT_COLUMNS = "sortColumns";
	private static final String VUE_SUMMARY_SELECTION = "summarySelection";
	private static final String VUE_OPERATOR = "operator";
	private static final String VUE_CONSTRAINTS = "constraints";
	private static final String VUE_VALUE = "value";
	private static final String VUE_MATCH_MODE = "matchMode";
	private static final String VUE_SMART_CLIENT_CRITERIA = "smartClientCriteria";
	
	private static final Set<SmartClientFilterOperator> VUE_ALLOWED_CONSTRAINT_OPERATORS = new TreeSet<>();
	static {
		VUE_ALLOWED_CONSTRAINT_OPERATORS.add(SmartClientFilterOperator.iStartsWith);
		VUE_ALLOWED_CONSTRAINT_OPERATORS.add(SmartClientFilterOperator.iContains);
		VUE_ALLOWED_CONSTRAINT_OPERATORS.add(SmartClientFilterOperator.iNotContains);
		VUE_ALLOWED_CONSTRAINT_OPERATORS.add(SmartClientFilterOperator.iEndsWith);
		VUE_ALLOWED_CONSTRAINT_OPERATORS.add(SmartClientFilterOperator.iEquals);
		VUE_ALLOWED_CONSTRAINT_OPERATORS.add(SmartClientFilterOperator.iNotEqual);

		VUE_ALLOWED_CONSTRAINT_OPERATORS.add(SmartClientFilterOperator.equals);
		VUE_ALLOWED_CONSTRAINT_OPERATORS.add(SmartClientFilterOperator.notEqual);
		VUE_ALLOWED_CONSTRAINT_OPERATORS.add(SmartClientFilterOperator.lessThan);
		VUE_ALLOWED_CONSTRAINT_OPERATORS.add(SmartClientFilterOperator.lessOrEqual);
		VUE_ALLOWED_CONSTRAINT_OPERATORS.add(SmartClientFilterOperator.greaterThan);
		VUE_ALLOWED_CONSTRAINT_OPERATORS.add(SmartClientFilterOperator.greaterOrEqual);
	}
	
	@Override
	public Snapshot fromClientPayload(String payload) {
		User u = CORE.getUser();
		
		Snapshot result = null;
		try {
			@SuppressWarnings("unchecked")
			Map<String, Object> vue = (Map<String, Object>) JSON.unmarshall(u, payload);
			result = new Snapshot();

			// NB No advanced
			
			// Filter and Columns
			
			// Filters
			Object value = vue.get(VUE_FILTERS);
			if (value instanceof Map) {
				@SuppressWarnings("unchecked")
				Map<String, Object> filters = (Map<String, Object>) value;
				
				// Column Widths
				value = vue.get(VUE_COLUMN_WIDTHS);
				if (value instanceof List) {
					@SuppressWarnings("unchecked")
					List<Number> widths = (List<Number>) value;

					// Visible Columns
					value = vue.get(VUE_VISIBLE_COLUMNS);
					if (value instanceof List) {
						@SuppressWarnings("unchecked")
						List<String> visible = (List<String>) value;
						if (visible.isEmpty()) { // all columns are showing
							visible = List.copyOf(filters.keySet());
						}

						// Populate columns
						for (int i = 0, vl = visible.size(), wl = widths.size(); i < vl; i++) {
							if (i < wl) {
								result.putColumn(visible.get(i), widths.get(i).intValue());
							}
							else {
								result.putColumn(visible.get(i));
							}
						}
						
						value = vue.get(VUE_OPERATOR);
						if (value instanceof String) {
							// Populate Filter
							SnapshotCriteria top = new SnapshotCriteria();
							top.setOperator(CompoundFilterOperator.valueOf((String) value));
							List<SnapshotFilter> topFilters = top.getFilters();
							
							// Default to radio search type unless we find some 2 level nesting
							AdvancedSearchType advancedSearchType = AdvancedSearchType.radio;
							
							for (Entry<String, Object> entry : filters.entrySet()) {
								Object v = entry.getValue();
								// Determine if there is anything to filter
								if (v instanceof Map) {
									@SuppressWarnings("unchecked")
									Map<String, Object> filter = (Map<String, Object>) v;
									value = filter.get(VUE_CONSTRAINTS);
									if (value instanceof List) {
										@SuppressWarnings("unchecked")
										List<Map<String, Object>> constraints = (List<Map<String, Object>>) value;
										// remove any filter constraints with null values
										constraints = constraints.stream().filter(e -> (e.get(VUE_VALUE) != null)).collect(Collectors.toList());
										int size = constraints.size();
	
										// Simple criteria
										String k = entry.getKey();
										if (size == 1) {
											Map<String, Object> constraint = constraints.get(0);
											SnapshotCriterion criterion = criterion(k, constraint);
											if (criterion != null) {
												topFilters.add(criterion);
											}
										}
										// Advanced Criteria
										else {
											SnapshotCriteria advanced = new SnapshotCriteria();
											// Operator
											value = filter.get(VUE_OPERATOR);
											if (value instanceof String) {
												advanced.setOperator(CompoundFilterOperator.valueOf((String) value));
	
												List<SnapshotFilter> advancedFilters = advanced.getFilters();
												constraints.forEach(e -> {
													SnapshotCriterion criterion = criterion(k, e);
													if (criterion != null) {
														advancedFilters.add(criterion);
													}
												});
												// Only add the advanced filter if it has filters defined
												if (! advancedFilters.isEmpty()) {
													topFilters.add(advanced);
													// More than 1 constraint means we need nested mode
													if (advancedFilters.size() > 1) {
														advancedSearchType = AdvancedSearchType.bracket;
													}
												}
											}
											else {
												throw new IllegalStateException("Malformed filter constraint operator in snapshot - " + value);
											}
										}
									}
									else {
										throw new IllegalStateException("Malformed filter constraints in snapshot - " + value);
									}
								}
								else {
									throw new IllegalStateException("Malformed filters in snapshot - " + v);
								}
							}
							
							// Set the filter and the advanced search type if we have something defined
							if (! topFilters.isEmpty()) {
								result.setFilter(top);
								result.setAdvanced(advancedSearchType);
							}
						}
						else {
							throw new IllegalStateException("Malformed operator in snapshot - " + value);
						}
					}
					else {
						throw new IllegalStateException("Malformed visibleColumns in snapshot - " + value);
					}
				}
				else {
					throw new IllegalStateException("Malformed columnWidths in snapshot - " + value);
				}
			}
			else {
				throw new IllegalStateException("Malformed filters in snapshot - " + value);
			}

			
			// NB No Group
			
			// Sorts
			value = vue.get(VUE_SORT_COLUMNS);
			if (value instanceof List) {
				@SuppressWarnings("unchecked")
				List<String> sortColumns = (List<String>) value;
				for (String sortColumn : sortColumns) {
					String column = UtilImpl.processStringValue(sortColumn);
					if (column != null) {
						if (column.charAt(0) == '-') {
							result.putSort(column.substring(1), SortDirection.descending);
						}
						else {
							result.putSort(column);
						}
					}
				}
			}
			else {
				throw new IllegalStateException("Malformed sortColumns in snapshot - " + value);
			}
			
			// Summary
			value = vue.get(VUE_SUMMARY_SELECTION);
			if (value instanceof String) {
				String summary = UtilImpl.processStringValue((String) value);
				if (summary != null) {
					result.setSummary(AggregateFunction.valueOf(summary));
				}
			}
			else {
				throw new IllegalStateException("Malformed summarySelection in snapshot - " + value);
			}
		}
		catch (Exception e) {
			LOGGER.warn("Snapshot could not be created from Vue Payload {}", payload, e);
			result = null;
		}
		
		return result;
	}

	private static SnapshotCriterion criterion(String column, Map<String, Object> constraint) {
		SnapshotCriterion result = null;

		Object value = constraint.get(VUE_MATCH_MODE);
		if (value instanceof String) {
			result = new SnapshotCriterion();
			result.setColumn(column);
			result.setOperator(SmartClientFilterOperator.valueOf((String) value));
			result.setValue(constraint.get(VUE_VALUE));
		}
		else {
			throw new IllegalStateException("Malformed filter constraint matchMode in snapshot - " + value);
		}
		
		return result;
	}
	
	@Override
	public String toClientPayload(Snapshot snapshot) {
		try {
			Map<String, Object> result = new LinkedHashMap<>(6);
			result.put(VUE_OPERATOR, CompoundFilterOperator.and); // Default operator

			// Add filter
			SnapshotFilter filter = snapshot.getFilter();
			if (filter != null) {
				Map<String, Object> filters = filters(filter);
				// Cannot convert snapshot filter to vue filter definition
				if (filters == null) {
					// If this was a SmartClient snapshot definition, send that...
					Map<String, Object> sourceSmartClientCriteria = snapshot.getSourceSmartClientCriteria();
					if (sourceSmartClientCriteria != null) {
						LOGGER.warn("SmartClient Filter for snapshot could not be converted to a Vue Payload - using original SmartClient Filter :- " + sourceSmartClientCriteria);
						result.put(VUE_SMART_CLIENT_CRITERIA, sourceSmartClientCriteria);
					}
					else {
						throw new IllegalStateException("Vue Filter could not be converted to a Vue payload");
					}
				}
				else {
					// Put the filters and the top level operator in the payload
					result.put(VUE_FILTERS, filters);
					if (filter instanceof SnapshotCriteria) {
						result.put(VUE_OPERATOR, ((SnapshotCriteria) filter).getOperator());
					}
				}
			}
			
			// Add Visible Columns and Column Widths
			Map<String, Integer> columns = snapshot.getColumns();
			List<String> visibleColumns = new ArrayList<>(columns.size());
			List<Integer> columnWidths = new ArrayList<>(columns.size());
			boolean allWidthsPresent = true;
			for (Entry<String, Integer> entry : columns.entrySet()) {
				visibleColumns.add(entry.getKey());
				Integer width = entry.getValue();
				if (width == null) {
					allWidthsPresent = false;
				}
				else {
					columnWidths.add(width);
				}
			}
			result.put(VUE_VISIBLE_COLUMNS, visibleColumns);
			if (allWidthsPresent) {
				columnWidths.clear();
			}
			result.put(VUE_COLUMN_WIDTHS, columnWidths);
	
			// Add Summary Selection
			AggregateFunction summary = snapshot.getSummary();
			result.put(VUE_SUMMARY_SELECTION, (summary == null) ? "" : summary.toString());
			
			
			// Add Sort Columns
			Map<String, SortDirection> sorts = snapshot.getSorts();
			List<String> sortColumns = new ArrayList<>(sorts.size());
			sorts.forEach((k, v) -> {
				sortColumns.add(SortDirection.descending.equals(v) ? 
									new StringBuilder(k.length() + 1).append('-').append(k).toString() :
									k);
			});
			result.put(VUE_SORT_COLUMNS, sortColumns);

			return JSON.marshall(result);
		}
		catch (Exception e) {
			LOGGER.error("Vue Filter for snapshot could not be converted to a Vue Payload - " + e.getLocalizedMessage());
			return null;
		}
	}
	
	/**
	 * Creates a Map of column -> filter criteria similar to
	 * {"contactType": {"operator": "and", "constraints": [{"value": "Person", "matchMode": "equals"}]}
	 * when marshalled.
	 * 
	 * Not all possible criteria can be restructured in this manner and if the filter parameter passed cannot be then null is returned.
	 * Filter crtieria that can be restructured conform to the following
	 *   - top level needs to be 
	 *     - a criterion or 
	 *     - a criteria with an and operator - its filters needs to be
	 *       - a Criterion (multiple criterion can be anded together under the same filter key
	 *       - a Criteria - all filters of this criteria need to be criterion on the same field.
	 * 
	 * @param filter	The filter to restructure and create Vue JSON filter criteria for.
	 * @return	The filter criteria map or null if restructuring is not possible.
	 */
	private static @Nullable Map<String, Object> filters(SnapshotFilter filter) {
		Map<String, Object> result = null;

		// A Criteria
		if (filter instanceof SnapshotCriteria) {
			SnapshotCriteria criteria = (SnapshotCriteria) filter;

			Map<String, SnapshotCriteria> restructured = restructure(criteria);
			if (restructured != null) { // null if not possible to restructure
				result = new LinkedHashMap<>(restructured.size());
				
				for (Entry<String, SnapshotCriteria> entry : restructured.entrySet()) {
					String column = entry.getKey();
					SnapshotCriteria restructuredConstraint = entry.getValue();
					Map<String, Object> vueFilter = new LinkedHashMap<>(2);
					vueFilter.put(VUE_OPERATOR, restructuredConstraint.getOperator());
					List<SnapshotFilter> restructuredConstraintFilters = restructuredConstraint.getFilters();
					List<Map<String, Object>> vueConstraints = new ArrayList<>(restructuredConstraintFilters.size());
					vueFilter.put(VUE_CONSTRAINTS, vueConstraints);
					result.put(column,  vueFilter);
					
					// Note - this only contains valid criterions
					for (SnapshotFilter restructuredConstraintFilter : restructuredConstraintFilters) {
						SnapshotCriterion restructuredConstraintCriterion = (SnapshotCriterion) restructuredConstraintFilter;
						vueConstraints.add(constraint(restructuredConstraintCriterion));
					}
				}
			}
		}
		// A Criterion
		else {
			SnapshotCriterion criterion = (SnapshotCriterion) filter;
			Map<String, Object> constraint = constraint(criterion);
			// Check for invalid constraint
			if (constraint != null) {
				Map<String, Object> filterObject = new LinkedHashMap<>(2);
				filterObject.put(VUE_OPERATOR, CompoundFilterOperator.and.toString());
				List<Map<String, Object>> constraints = new ArrayList<>(1);
				constraints.add(constraint);
				filterObject.put(VUE_CONSTRAINTS, constraints);
				result = new LinkedHashMap<>();
				result.put(criterion.getColumn(), filterObject);
			}
		}
		
		return result;
	}
	
	/**
	 * Restructure filter criteria to fit The VueListGrid.
	 * Returns null if not able to. 
	 * @param topCriteria	
	 * @return	Restructured 3-tier and criteria -> and/or criteria -> criterion(s) or null if impossible.
	 */
	private static @Nullable Map<String, SnapshotCriteria> restructure(SnapshotCriteria topCriteria) {
		CompoundFilterOperator operator = topCriteria.getOperator();
		// Check the operator - Not is not supported
		if ((operator == null) || CompoundFilterOperator.not.equals(operator)) {
			return null;
		}
		
		Map<String, SnapshotCriteria> result = new LinkedHashMap<>();

		for (SnapshotFilter middleFilter : topCriteria.getFilters()) {
			// A criterion
			if (middleFilter instanceof SnapshotCriterion) {
				SnapshotCriterion middleCriterion = (SnapshotCriterion) middleFilter;
				// start and end not supported
				if ((middleCriterion.getStart() != null) || (middleCriterion.getEnd() != null)) {
					return null;
				}
				// not a supported operator
				SmartClientFilterOperator middleCriterionOperator = middleCriterion.getOperator();
				if ((middleCriterionOperator == null) || (! VUE_ALLOWED_CONSTRAINT_OPERATORS.contains(middleCriterionOperator))) {
					return null;
				}

				String column = middleCriterion.getColumn();
				SnapshotCriteria criteria = result.get(column);
				if (criteria == null) {
					criteria = new SnapshotCriteria(); // default operator is "and" which is correct here
					result.put(column, criteria);
				}
				else {
					// A nested OR criteria has been added that is incompatible
					if (! CompoundFilterOperator.and.equals(criteria.getOperator())) {
						return null;
					}
				}
				criteria.getFilters().add(middleCriterion);
			}
			// A criteria
			else {
				SnapshotCriteria middleCriteria = (SnapshotCriteria) middleFilter;
				// Not not supported
				CompoundFilterOperator middleCriteriaOperator = middleCriteria.getOperator();
				if ((middleCriteriaOperator == null) || CompoundFilterOperator.not.equals(middleCriteriaOperator)) {
					return null;
				}

				String column = null;
				for (SnapshotFilter bottomFilter : middleCriteria.getFilters()) {
					// Infinite nesting is not supported
					if (bottomFilter instanceof SnapshotCriteria) {
						return null;
					}
					
					SnapshotCriterion bottomCriterion = (SnapshotCriterion) bottomFilter;
					// start and end not supported
					if ((bottomCriterion.getStart() != null) || (bottomCriterion.getEnd() != null)) {
						return null;
					}
					// not a supported operator
					SmartClientFilterOperator bottomCriterionOperator = bottomCriterion.getOperator();
					if ((bottomCriterionOperator == null) || (! VUE_ALLOWED_CONSTRAINT_OPERATORS.contains(bottomCriterionOperator))) {
						return null;
					}
					
					// Capture the column name
					if (column == null) {
						column = bottomCriterion.getColumn();
					}
					// Heterogeneous nested column names not supported
					else if (! column.equals(bottomCriterion.getColumn())) {
						return null;
					}
					
					SnapshotCriteria criteria = result.get(column);
					if (criteria != null) {
						// A nested criteria has been added that is incompatible
						if (! middleCriteriaOperator.equals(criteria.getOperator())) {
							return null;
						}
					}
					else {
						criteria = new SnapshotCriteria();
						criteria.setOperator(middleCriteriaOperator);
						result.put(column, criteria);
					}

					criteria.getFilters().add(bottomCriterion);
				}
			}
		}
		return result;
	}

	private static Map<String, Object> constraint(SnapshotCriterion criterion) {
		// Cannot convert start and end (at this stage)
		Object start = criterion.getStart();
		Object end = criterion.getEnd();
		if ((start != null) || (end != null)) {
			throw new IllegalStateException("No support for range constraints");
		}

		Map<String, Object> result = new LinkedHashMap<>();
		result.put(VUE_VALUE, criterion.getValue());
		result.put(VUE_MATCH_MODE, criterion.getOperator());
		return result;
	}
}
