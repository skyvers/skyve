package org.skyve.wildcat.web.service.smartclient;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.Query;
import org.skyve.metadata.module.query.QueryColumn;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.domain.MapBean;
import org.skyve.wildcat.domain.messages.SecurityException;
import org.skyve.wildcat.metadata.model.document.field.ConvertableField;
import org.skyve.wildcat.metadata.model.document.field.Enumeration;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.persistence.AbstractDocumentQuery;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.JSONUtil;
import org.skyve.wildcat.util.TagUtil;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.AbstractWebContext;
import org.skyve.wildcat.web.ServletConstants;
import org.skyve.wildcat.web.SortParameter;
import org.skyve.wildcat.web.WebUtil;

import com.vividsolutions.jts.geom.Geometry;

/**
 * Service for list views.
 */
public class SmartClientListServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	
	static final String ISC_META_DATA_PREFIX = "isc_metaDataPrefix";
	static final String ISC_DATA_FORMAT = "isc_dataFormat";
	static final String OLD_VALUES = "_oldValues";

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) 
	throws ServletException, IOException {
		UtilImpl.LOGGER.info("SmartClientList - get....");
		processRequest(request, response);
	}
	
	@Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) 
	throws ServletException, IOException {
		UtilImpl.LOGGER.info("SmartClientList - post....");
		processRequest(request, response);
	}
	
	// NB - Never throw ServletException as this will halt the SmartClient Relogin flow.
    private static void processRequest(HttpServletRequest request, HttpServletResponse response) 
	throws IOException {

        response.setContentType(MimeType.json.toString());
        response.setCharacterEncoding(ServletConstants.UTF8);
		response.addHeader("Cache-control", "private,no-cache,no-store"); // never
		response.addDateHeader("Expires", 0); // never

		try (PrintWriter pw = response.getWriter()) {
			String dataSource = request.getParameter("_dataSource");
			String operationType = request.getParameter("_operationType");
	        if (operationType == null) {
	        	pw.append("{}");
	        	return;
	        }
	        Operation operation = Operation.valueOf(operationType);
	        AbstractPersistence persistence = null;
	        
	        try {
				try {
					// use the view's conversation if it was sent down from the client
					String webId = request.getParameter(AbstractWebContext.CONTEXT_NAME);
					AbstractWebContext webContext = WebUtil.getCachedConversation(webId, request, response);
					if (webContext != null) {
			        	UtilImpl.LOGGER.info("USE VIEW CONVERSATION!!!!");
			            persistence = webContext.getConversation();
			            persistence.setForThread();
			        }
			        // if no conversation to use, start a new one
			        if (persistence == null) {
			            persistence = AbstractPersistence.get();
			            persistence.evictAllCached();
			        }

			        persistence.begin();
			    	Principal userPrincipal = request.getUserPrincipal();
			    	User user = WebUtil.processUserPrincipalForRequest(request, (userPrincipal == null) ? null : userPrincipal.getName(), true);
					if (user == null) {
						throw new SessionEndedException();
					}
			    	persistence.setUser(user);
	
			    	Customer customer = user.getCustomer();
			        Module module = null;
					Query query = null;
	
			        if (dataSource != null) {
			        	// use first 2 tokens of '_' split - could be a pick list which means extra '_' in it
			        	String[] tokens = dataSource.split("_");
						module = customer.getModule(tokens[0]);
						String documentOrQueryName = tokens[1];
						query = module.getQuery(documentOrQueryName);
						if (query == null) {
							query = module.getDocumentDefaultQuery(customer, documentOrQueryName);
						}
						if (query == null) {
							throw new ServletException("DataSource does not reference a valid query " + documentOrQueryName);
						}
					}
			        else {
			        	throw new ServletException("No datasource name in the request.");
			        }
			        Document queryDocument = module.getDocument(customer, query.getDocumentName());
			        
			        SortedMap<String, Object> parameters = new TreeMap<>();
					java.util.Enumeration<String> names = request.getParameterNames();
					while (names.hasMoreElements()) {
						String name = names.nextElement();
						if (ISC_META_DATA_PREFIX.equals(name) || ISC_DATA_FORMAT.equals(name)) {
							continue;
						}
						String value = request.getParameter(name);
						if (name.charAt(0) != '_') {
							// no '.' allowed in smart client field names
							name = name.replace('_', '.');
							
							// "null" can be sent by Smart Client
							if (value != null) {
								if ((value.length() == 0) || "null".equals(value)) {
									value = null;
								}
							}
							parameters.put(name, value);
						}
					}
					for (String name : parameters.keySet()) {
						UtilImpl.LOGGER.info(name + " = " + parameters.get(name));
					}

					switch (operation) {
					case fetch:
						if (! user.canReadDocument(queryDocument)) {
							throw new SecurityException("read this data", user.getName());
						}
						String _startRow = request.getParameter("_startRow");
						int startRow = (_startRow == null) ? 0 : Integer.parseInt(_startRow);
						String _endRow = request.getParameter("_endRow");
						int endRow = (_endRow == null) ? Integer.MAX_VALUE : Integer.parseInt(_endRow);
						String textMatchStyle = request.getParameter("_textMatchStyle");
						SmartClientFilterOperator operator = null;
						if (textMatchStyle != null) {
							operator = SmartClientFilterOperator.valueOf(textMatchStyle);
						}
	
						
						String[] sortBys = request.getParameterValues("_sortBy");
						SortParameter[] sortParameters = null;
						if (sortBys != null) {
							sortParameters = new SortParameter[sortBys.length];
							for (int i = 0; i < sortBys.length; i++) {
								String sortBy = sortBys[i];
								
								SortParameter sortParameter = new SortParameter();
								if (sortBy.startsWith("-")) {
									sortParameter.setDirection(SortDirection.descending);
									sortBy = sortBy.substring(1);
								}
								else {
									sortParameter.setDirection(SortDirection.ascending);
								}
								sortParameter.setBinding(sortBy.replace('_', '.'));
								
								sortParameters[i] = sortParameter;
							}
						}
	
						String summary = request.getParameter("_summary");
						if ("".equals(summary)) {
							summary = null;
						}
	
						fetch(module,
								queryDocument,
								query, 
								startRow, 
								endRow, 
								operator, 
								request.getParameterValues("criteria"),
								sortParameters,
								(summary == null) ? null : AggregateFunction.valueOf(summary),
								// include a summary extra row (for list grids)
								request.getParameterMap().containsKey("_summary"),
								request.getParameter("_tagId"),
								parameters, 
								persistence, 
								pw);
						break;
					case add:
						if (! user.canCreateDocument(queryDocument)) {
							throw new SecurityException("create this data", user.getName());
						}
						break;
					case update:
						String tagId = request.getParameter("_tagId");
						String bizTagged = (String) parameters.get(PersistentBean.TAGGED_NAME);
						if ("TAG".equals(bizTagged)) {
							tag(customer, module, query, tagId, parameters, pw);
						}
						else if ("UNTAG".equals(bizTagged)) {
							untag(customer, module, query, tagId, parameters, pw);
						}
						else {
							if (! user.canUpdateDocument(queryDocument)) {
								throw new SecurityException("update this data", user.getName());
							}
		
							boolean rowIsTagged = false;
							String oldValuesJSON = request.getParameter(OLD_VALUES);
							if (oldValuesJSON != null) {
								rowIsTagged = oldValuesJSON.contains(PersistentBean.TAGGED_NAME + "\":true");
							}
							update(module, 
									query, 
									tagId,
									rowIsTagged,
									parameters, 
									persistence, 
									pw);
						}
						break;
					case remove:
						if (! user.canDeleteDocument(queryDocument)) {
							throw new SecurityException("delete this data", user.getName());
						}
	
						remove(module, 
								query, 
								parameters, 
								persistence, 
								pw);
						break;
					default:
					}
					
					// serialize and cache conversation, if applicable
			    	if (webContext != null) {
			    		WebUtil.putConversationInCache(webContext);
			    	}
				}
				catch (InvocationTargetException e) {
					throw e.getTargetException();
				}
			}
			catch (Throwable t) {
		    	t.printStackTrace();
		    	if (persistence != null) {
		    		persistence.rollback();
		    	}
	
		    	SmartClientEditServlet.produceErrorResponse(t, operation, false, pw);
			}
		    finally {
		    	if (persistence != null) {
		    		persistence.commit(true);
		    	}
		    }
		}
	}
	
    private static void fetch(Module module,
    							Document queryDocument,
		    					Query query, 
								int startRow,
								int endRow,
								SmartClientFilterOperator operator,
								String[] criteria,
								SortParameter[] sortParameters,
								AggregateFunction summaryType,
								boolean includeExtraSummaryRow,
								String tagId,
								SortedMap<String, Object> parameters,
								AbstractPersistence persistence,
								PrintWriter pw)
	throws Exception {
		StringBuilder message = new StringBuilder(256);
		
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Set<String> propertyNames = new TreeSet<>();
		propertyNames.add(Bean.DOCUMENT_ID);
		propertyNames.add(PersistentBean.LOCK_NAME);
		propertyNames.add(PersistentBean.TAGGED_NAME);
		propertyNames.add(PersistentBean.FLAG_COMMENT_NAME);
		propertyNames.add(Bean.BIZ_KEY);
		for (QueryColumn column : query.getColumns()) {
			if (column.isProjected()) {
				String binding = column.getBinding();
				// if this binding is to an association, 
				// add the bizId as the column value and bizKey as the column displayValue
				TargetMetaData target = Binder.getMetaDataForBinding(customer,
																		module,
																		queryDocument,
																		binding);
				
				if (target.getAttribute() instanceof Association) {
					StringBuilder sb = new StringBuilder(64);
					sb.append(binding).append('.').append(Bean.BIZ_KEY);
					propertyNames.add(sb.toString());
				}
				propertyNames.add((binding != null) ? binding : column.getName());
			}
		}
		
		DocumentQuery summaryQuery = null;
		DocumentQuery detailQuery = query.constructDocumentQuery(null, tagId);

		Document document = detailQuery.getDrivingDocument();
		
		// Add filter criteria to query
		addFilterCriteriaToQuery(module, document, detailQuery, user, operator, criteria, parameters, tagId);

		// Apply sort - if defined
		if (sortParameters != null) {
			for (SortParameter sortParameter : sortParameters) {
				String binding = sortParameter.getBinding();
				SortDirection direction = sortParameter.getDirection();
				detailQuery.insertOrdering(binding, direction);
			}
		}

		detailQuery.setFirstResult(startRow);
		detailQuery.setMaxResults(endRow - startRow);
		List<Bean> beans = detailQuery.projectedResults();
		if (summaryType == null) {
			summaryQuery = query.constructDocumentQuery(AggregateFunction.Count, tagId);
			addFilterCriteriaToQuery(module, document, summaryQuery, user, operator, criteria, parameters, tagId);
			AbstractDocumentQuery internalSummaryQuery = (org.skyve.wildcat.persistence.AbstractDocumentQuery) summaryQuery;
			internalSummaryQuery.clearProjections();
			internalSummaryQuery.clearOrderings();
		}
		else {
			summaryQuery = query.constructDocumentQuery(summaryType, tagId);
			addFilterCriteriaToQuery(module, document, summaryQuery, user, operator, criteria, parameters, tagId);
		}

		// This needs to be the ID to satisfy the client data source definitions
		summaryQuery.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, Bean.DOCUMENT_ID);
		summaryQuery.addAggregateProjection(AggregateFunction.Min, PersistentBean.FLAG_COMMENT_NAME, PersistentBean.FLAG_COMMENT_NAME);
		
		Bean summaryBean = summaryQuery.projectedResults().get(0);
		if (includeExtraSummaryRow) {
			BindUtil.set(summaryBean, PersistentBean.FLAG_COMMENT_NAME, summaryType);
			beans.add(summaryBean);
		}
		
		long totalRows = ((Number) BindUtil.get(summaryBean, Bean.DOCUMENT_ID)).longValue();
		message.append("{response:{");
		message.append("status:0,");
		message.append("startRow:").append(startRow);
		message.append(",endRow:");
		message.append(Math.min(totalRows, endRow));
		message.append(",totalRows:");
		message.append(totalRows);
		message.append(",data:");
		message.append(JSONUtil.marshall(customer, beans, propertyNames));
		message.append("}}");
		pw.append(message);
	}
    
	private static void addFilterCriteriaToQuery(Module module,
													Document document,
													DocumentQuery query,
		    										User user,
		    										SmartClientFilterOperator filterOperator,
		    										String[] criteria,
		    										SortedMap<String, Object> parameters,
		    										String tagId) 
    throws Exception {
    	SortedMap<String, Object> mutableParameters = new TreeMap<>(parameters);
    	CompoundFilterOperator compoundFilterOpererator = CompoundFilterOperator.and;
    	String operatorParameter = (String) mutableParameters.get("operator");
    	if (operatorParameter != null) { // advanced criteria
    		compoundFilterOpererator = CompoundFilterOperator.valueOf(operatorParameter);

    		// Produce advanced criteria parameter
    		List<Map<String, Object>> advancedCriteria = null;
    		if (criteria == null) {
    			advancedCriteria = new ArrayList<>(0);
    		}
    		else {
    			advancedCriteria = new ArrayList<>(criteria.length);
        		for (String jsonCriteria : criteria) {
    				// Get each criterium name, operator and operands
					@SuppressWarnings("unchecked")
					Map<String, Object> criterium = (Map<String, Object>) JSONUtil.unmarshall(user, jsonCriteria);
    				advancedCriteria.add(criterium);
        		}
    		}
    		addAdvancedFilterCriteriaToQuery(module,
    											document,
    											user,
    											query,
    											compoundFilterOpererator,
    											advancedCriteria,
    											tagId);
    		
    		mutableParameters.remove("operator");
    		mutableParameters.remove("criteria");
    	}

    	// simple criteria or extra criteria from grid filter parameters
    	addSimpleFilterCriteriaToQuery(module,
										document,
										user,
										query,
										filterOperator,
										mutableParameters,
										tagId);
    }

    /**
     * Add simple criteria to the query.
     * 
     * @param module
     * @param document
     * @param user
     * @param query	Add the filter critiera to this query.
     * @param filterOperator	The default operator to use for all filter critiera.
     * @param criteria	A Map of name value pairs
     * @throws Exception
     */
    public static void addSimpleFilterCriteriaToQuery(Module module,
														Document document,
														User user,
														DocumentQuery query,
			    										SmartClientFilterOperator filterOperator,
			    										Map<String, Object> criteria,
			    										String tagId) 
    throws Exception {
		DocumentFilter topLevelFilter = query.getFilter();
		for (String binding : criteria.keySet()) {
			Object value = criteria.get(binding);
			if ("".equals(value)) {
				value = null;
			}
			
			binding = binding.replace('_', '.');
			boolean queryParameter = (binding.charAt(0) == ':');
			if (queryParameter) {
				binding = binding.substring(1); // lose the colon
			}
			
			// Determine the type and converter of the filtered attribute
			Customer customer = user.getCustomer();
			Converter<?> converter = null;
    		Class<?> type = String.class;
    		
    		// Must be a valid property if we are adding a filter criteria
    		// Not necessarily a valid property binding if processing a query parameter
    		TargetMetaData target = null;
    		// set to true if equals should be used instead of substring
    		boolean equalsOperatorRequired = false;
    		try {
				target = BindUtil.getMetaDataForBinding(customer, 
															module, 
															document, 
															binding);
    		}
    		catch (MetaDataException e ) {
    			if (! queryParameter) {
    				throw e;
    			}
    		}
			if (target != null) {
				Attribute attribute = target.getAttribute();
				if (attribute != null) {
					if (attribute instanceof Enumeration) {
						type = AbstractRepository.get().getEnum((Enumeration) attribute);
						equalsOperatorRequired = true;
					}
					else {
						type = attribute.getAttributeType().getImplementingType();
					}
					if (attribute.getDomainType() != null) {
						equalsOperatorRequired = true;
					}
					if (attribute instanceof ConvertableField) {
						ConvertableField field = (ConvertableField) attribute;
						converter = field.getConverterForCustomer(customer);
					}
					else if (attribute instanceof Association) {
						type = String.class;
						StringBuilder sb = new StringBuilder(64);
						sb.append(binding).append('.').append(Bean.DOCUMENT_ID);
						binding = sb.toString();
						equalsOperatorRequired = true;
					}
				}
			}

			if (value != null) {
				value = fromString(binding, 
									"value", 
									value.toString(),
									customer, 
									converter, 
									type);
			}

			if (queryParameter) {
				query.putParameter(binding, value);
			}
			else {
				equalsOperatorRequired = equalsOperatorRequired || 
											(value instanceof Date) ||
											(value instanceof Number) ||
											(value instanceof Boolean);
				addCriteriumToFilter(topLevelFilter, 
										binding, 
										equalsOperatorRequired ? SmartClientFilterOperator.equals : filterOperator, 
										value, 
										null, 
										null,
										user,
										tagId);
			}
		}
	}

    /**
     * Add advanced filter criteria to a query.
     * @param module
     * @param document
     * @param customer
     * @param user
     * @param query	The query to add the filter criteria to.
     * @param compoundFilterOperator	The compound filter operator to use between criteria
     * @param criteria	List of advanced critiera.
     * @throws Exception
     */
	public static void addAdvancedFilterCriteriaToQuery(Module module,
															Document document,
															User user,
															DocumentQuery query,
															CompoundFilterOperator compoundFilterOperator,
															List<Map<String, Object>> criteria,
															String tagId)
	throws Exception {
		addAdvancedFilterCriteriaToQuery(module,
											document,
											user,
											query,
											query.getFilter(),
											compoundFilterOperator,
											criteria,
											tagId);
	}
    
	private static final String PARENT_NAME_SUFFIX = "." + ChildBean.PARENT_NAME;

	private static void addAdvancedFilterCriteriaToQuery(Module module,
															Document document,
															User user,
															DocumentQuery query,
															DocumentFilter currentFilter,
				    										CompoundFilterOperator compoundFilterOperator,
				    										List<Map<String, Object>> criteria,
				    										String tagId)
    throws Exception {
		if (criteria != null) {
			boolean firstCriteriaIteration = true; // the first filter criteria encountered - not a bound parameter
			for (Map<String, Object> criterium : criteria) {
System.out.println(criterium);
				String binding = ((String) criterium.get("fieldName"));
				if (binding != null) {
					binding = binding.replace('_', '.');
				}
				SmartClientFilterOperator filterOperator = SmartClientFilterOperator.valueOf((String) criterium.get("operator"));

				if (binding == null) { // advanced criteria
					DocumentFilter subFilter = query.newDocumentFilter();
					CompoundFilterOperator subCompoundFilterOperator = CompoundFilterOperator.valueOf(filterOperator.toString());
					@SuppressWarnings("unchecked")
					List<Map<String, Object>> subCritiera = (List<Map<String, Object>>) criterium.get("criteria");
					addAdvancedFilterCriteriaToQuery(module,
														document,
														user,
														query,
														subFilter,
														subCompoundFilterOperator,
														subCritiera,
														tagId);
					if (! subFilter.isEmpty()) {
						if (CompoundFilterOperator.or.equals(compoundFilterOperator)) {
							currentFilter.addDisjunction(subFilter);
						}
						else { // not is taken into account below
							currentFilter.addConjunction(subFilter);
						}
					}
				}
				else { // simple criteria
					Object value = criterium.get("value");
		    		String valueString = null;
		    		if (value != null) {
		    			valueString = value.toString();
			    		if ("".equals(valueString)) {
			    			valueString = null;
			    		}
		    		}
	
		    		boolean queryParameter = (binding.charAt(0) == ':');
		    		if (queryParameter) {
		    			binding = binding.substring(1);
		    		}
		    		
		    		// Determine the type and converter of the filtered attribute
		    		Customer customer = user.getCustomer();
		    		Converter<?> converter = null;
		    		Class<?> type = String.class;
		    		
		    		TargetMetaData target = null;
		    		try {
		    			target = BindUtil.getMetaDataForBinding(customer, 
																	module, 
																	document, 
																	binding);
		    		}
		    		catch (MetaDataException e) {
		    			if (! queryParameter) {
		    				throw e;
		    			}
		    		}
		    		if (target != null) {
	    				Attribute attribute = target.getAttribute();
	    				if (attribute != null) {
							if (attribute instanceof Enumeration) {
								type = AbstractRepository.get().getEnum((Enumeration) attribute);
							}
							else {
								type = attribute.getAttributeType().getImplementingType();
							}
	    					if (attribute instanceof ConvertableField) {
	    						ConvertableField field = (ConvertableField) attribute;
	    						converter = field.getConverterForCustomer(customer);
	    					}
	    					else if (attribute instanceof Association) {
	    						type = String.class;
	    	    				binding = new StringBuilder(binding.length() + 6).append(binding).append('.').append(Bean.DOCUMENT_ID).toString();
	    					}
	    				}
		    			else if (ChildBean.PARENT_NAME.equals(binding) || binding.endsWith(PARENT_NAME_SUFFIX)) {
		    				type = String.class;
		    				binding = new StringBuilder(binding.length() + 6).append(binding).append('.').append(Bean.DOCUMENT_ID).toString();
		    			}
	    			}
	    			
	    			value = fromString(binding, "value", valueString, customer, converter, type);
	
	    			if (queryParameter) {
						query.putParameter(binding, value);
						continue;
	    			}
	
		    		Object start = criterium.get("start");
		    		String startString = null;
		    		if (start != null) {
		    			startString = start.toString();
			    		if ("".equals(startString)) {
			    			startString = null;
			    		}
		    		}
		    		
		    		Object end = criterium.get("end");
		    		String endString = null;
		    		if (end != null) {
		    			endString = end.toString();
			    		if ("".equals(endString)) {
			    			endString = null;
			    		}
		    		}
	
		    		start = fromString(binding, "start", startString, customer, converter, type);
	    			end = fromString(binding, "end", endString, customer, converter, type);
	    			
		    		switch (compoundFilterOperator) {
		    		case and:
		    			addCriteriumToFilter(currentFilter, binding, filterOperator, value, start, end, user, tagId);
		    			break;
		    		case or:
		    			if (firstCriteriaIteration) {
		    				addCriteriumToFilter(currentFilter, binding, filterOperator, value, start, end, user, tagId);
		    			}
		    			else {
			    			DocumentFilter orFilter = query.newDocumentFilter();
			    			addCriteriumToFilter(orFilter, binding, filterOperator, value, start, end, user, tagId);
			    			if (! orFilter.isEmpty()) {
			    				currentFilter.addDisjunction(orFilter);
			    			}
		    			}
		    			break;
		    		case not:
		    			switch (filterOperator) {
		    			case substring:
		    			case iContains:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.iNotContains, value, start, end, user, tagId);
		    				break;
		    			case iNotContains:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.iContains, value, start, end, user, tagId);
		    				break;
		    			case startsWith:
		    			case iStartsWith:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.iNotStartsWith, value, start, end, user, tagId);
		    				break;
		    			case iNotStartsWith:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.iStartsWith, value, start, end, user, tagId);
		    				break;
		    			case iEndsWith:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.iNotEndsWith, value, start, end, user, tagId);
		    				break;
		    			case iNotEndsWith:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.iEndsWith, value, start, end, user, tagId);
		    				break;
		    			case exact:
		    			case equals:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.notEqual, value, start, end, user, tagId);
		    				break;
		    			case iEquals:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.iNotEqual, value, start, end, user, tagId);
		    				break;
		    			case notEqual:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.equals, value, start, end, user, tagId);
		    				break;
		    			case iNotEqual:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.iEquals, value, start, end, user, tagId);
		    				break;
		    			case greaterThan:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.lessOrEqual, value, start, end, user, tagId);
		    				break;
		    			case greaterOrEqual:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.lessThan, value, start, end, user, tagId);
		    				break;
		    			case lessThan:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.greaterOrEqual, value, start, end, user, tagId);
		    				break;
		    			case lessOrEqual:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.greaterThan, value, start, end, user, tagId);
		    				break;
		    			case betweenInclusive:
		    			case iBetweenInclusive:
		    				if (start != null) {
				    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.lessOrEqual, start, null, null, user, tagId);
		    				}
		    				if (end != null) {
				    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.greaterThan, end, null, null, user, tagId);
		    				}
		    				break;
		    			case isNull:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.notNull, value, start, end, user, tagId);
		    				break;
		    			case notNull:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.isNull, value, start, end, user, tagId);
		    				break;
		    			case equalsField:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.notEqualField, value, start, end, user, tagId);
		    				break;
		    			case notEqualField:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.equalsField, value, start, end, user, tagId);
		    				break;
		    			case greaterThanField:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.lessOrEqualField, value, start, end, user, tagId);
		    				break;
		    			case greaterOrEqualField:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.lessThanField, value, start, end, user, tagId);
		    				break;
		    			case lessThanField:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.greaterOrEqualField, value, start, end, user, tagId);
		    				break;
		    			case lessOrEqualField:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.greaterThanField, value, start, end, user, tagId);
		    				break;
						case inSet:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.notInSet, value, start, end, user, tagId);
		    				break;
						case notInSet:
			    			addCriteriumToFilter(currentFilter, binding, SmartClientFilterOperator.inSet, value, start, end, user, tagId);
		    				break;
						case regexp:
						case iregexp:
							// nothing to do
							break;
						case gContains:
						case gCrosses:
						case gDisjoint:
						case gEquals:
						case gIntersects:
						case gOverlaps:
						case gTouches:
						case gWithin:
							// TODO fix this later
							break;
						case and:
						case or:
						case not:
							// nothing to do
							break;
						default:
		    			}
		    			break;
					default:
		    		}
		    		firstCriteriaIteration = false;
				}
    		}
    	}
	}

    private static Object fromString(String valueBinding,
    									String valueDescription,
    									String valueString,
    									Customer customer,
    									Converter<?> converter,
    									Class<?> type)
    throws ValidationException {
    	Object result = null;
    	
		if (valueString != null) {
			try {
				// smart client can send dates (from filter builder) in the format YYYY-MM-DD without the T, so we'll add this
				if ((Date.class.isAssignableFrom(type)) && (valueString.indexOf('T') < 0)) {
					result = BindUtil.fromString(customer, converter, type, valueString + "T00:00:00.000", true);
				}
				else {
					result = BindUtil.fromString(customer, converter, type, valueString, true);
				}
			}
			catch (Exception e) {
				try {
					result = BindUtil.fromString(customer, converter, type, valueString, false);
				}
				catch (Exception e1) {
					e.printStackTrace();
					e1.printStackTrace();
					if (valueBinding == null) {
						throw new ValidationException(new Message("Please enter a properly formatted " + valueDescription));
					}
					throw new ValidationException(new Message(valueBinding.replace('.', '_'), "Please enter a properly formatted " + valueDescription));
				}
			}
		}

		if (result instanceof String) {
			result = ((String) result).replace('\'', '%'); // remove ' delimiters;
		}
		
		return result;
    }
    
    private static void addCriteriumToFilter(DocumentFilter filter, 
    											String binding,
    											SmartClientFilterOperator filterOperator, 
    											Object value,
    											Object start,
    											Object end,
    											User user,
    											String tagId) {
    	if (PersistentBean.TAGGED_NAME.equals(binding)) {
			StringBuilder sb = new StringBuilder(64);
			sb.append("exists (select 1 from adminTagged as tagged where tagged.tag.bizId = '");
			sb.append(tagId);
			sb.append("' and tagged.bizUserId = '");
			sb.append(user.getId());
			sb.append("' and tagged.taggedBizId = bean.bizId)");

			if ("true".equals(value)) {
    			filter.addExpression(sb.toString());
    		}
    		else if ("false".equals(value)) {
    			sb.insert(0, "not ");
    			filter.addExpression(sb.toString());
    		}
    	}
    	else if (filterOperator == null) {
			if (value != null) {
				filter.addEquals(binding, value);
			}
    	}
    	else {
    		Object revisedValue = value;
    		if (value instanceof org.skyve.domain.types.Enumeration) {
    			revisedValue = ((org.skyve.domain.types.Enumeration) value).toCode();
    		}
    		switch (filterOperator) {
    		case substring:
    		case iContains:
    			if (revisedValue != null) {
    				StringBuilder sb = new StringBuilder(32);
    				filter.addLike(binding, sb.append('%').append(revisedValue).append('%').toString());
    			}
    			break;
    		case iNotContains:
    			if (revisedValue != null) {
    				StringBuilder sb = new StringBuilder(32);
    				filter.addNullOrNotLike(binding, sb.append('%').append(revisedValue).append('%').toString());
    			}
    			break;
    		case startsWith:
    		case iStartsWith:
    			if (revisedValue != null) {
    				StringBuilder sb = new StringBuilder(32);
    				filter.addLike(binding, sb.append(revisedValue).append('%').toString());
    			}
    			break;
    		case iNotStartsWith:
    			if (revisedValue != null) {
    				StringBuilder sb = new StringBuilder(32);
    				filter.addNullOrNotLike(binding, sb.append(revisedValue).append('%').toString());
    			}
    			break;
    		case iEndsWith:
    			if (revisedValue != null) {
    				StringBuilder sb = new StringBuilder(32);
    				filter.addLike(binding, sb.append('%').append(revisedValue).toString());
    			}
    			break;
    		case iNotEndsWith:
    			if (revisedValue != null) {
    				StringBuilder sb = new StringBuilder(32);
    				filter.addNullOrNotLike(binding, sb.append('%').append(revisedValue).toString());
    			}
    			break;
    		case equals:
    		case exact:
    			if (revisedValue != null) {
    				filter.addEquals(binding, value);
    			}
    			break;
    		case iEquals:
    			if (value != null) {
    				filter.addLike(binding, revisedValue.toString());
    			}
    			break;
    		case notEqual:
    			if (value != null) {
    				filter.addNotEquals(binding, value);
    			}
    			break;
    		case iNotEqual:
    			if (value != null) {
    				filter.addNotLike(binding, revisedValue.toString());
    			}
    			break;
    		case greaterThan:
    			if (value != null) {
    				filter.addGreaterThan(binding, value);
    			}
    			break;
    		case greaterOrEqual:
    			if (value != null) {
    				filter.addGreaterThanOrEqualTo(binding, value);
    			}
    			break;
    		case lessThan:
    			if (value != null) {
    				filter.addLessThan(binding, value);
    			}
    			break;
    		case lessOrEqual:
    			if (value != null) {
    				filter.addLessThanOrEqualTo(binding, value);
    			}
    			break;
    		case iBetweenInclusive:
    		case betweenInclusive:
    			if ((start != null) && (end != null)) {
    				filter.addBetween(binding, start, end);
    			}
    			else if (start != null) {
    				filter.addGreaterThanOrEqualTo(binding, start);
    			}
    			else if (end != null) {
    				filter.addLessThanOrEqualTo(binding, end);
    			}
    			break;
    		case isNull:
    			filter.addNull(binding);
    			break;
    		case notNull:
    			filter.addNotNull(binding);
    			break;
    		case equalsField:
// TODO
    			break;
    		case notEqualField:
// TODO
    			break;
			case greaterOrEqualField:
// TODO
				break;
			case greaterThanField:
// TODO
				break;
			case lessOrEqualField:
// TODO
				break;
			case lessThanField:
// TODO
				break;
    		case regexp: // Regular expression match
    		case iregexp: // Regular expression match (case insensitive)
    			break;
    		case inSet: // value is in a set of values. Specify criterion.value as an Array
    			break;
    		case notInSet: // value is not in a set of values. Specify criterion.value as an Array
    			break;
    		case gWithin:
				if (value instanceof Geometry) {
					filter.addWithin(binding, (Geometry) value);
				}
				break;
    		case gContains:
				if (value instanceof Geometry) {
					filter.addContains(binding, (Geometry) value);
				}
				break;
    		case gCrosses:
				if (value instanceof Geometry) {
					filter.addCrosses(binding, (Geometry) value);
				}
				break;
    		case gDisjoint:
				if (value instanceof Geometry) {
					filter.addDisjoint(binding, (Geometry) value);
				}
				break;
    		case gEquals:
				if (value instanceof Geometry) {
					filter.addEquals(binding, (Geometry) value);
				}
				break;
    		case gIntersects:
				if (value instanceof Geometry) {
					filter.addIntersects(binding, (Geometry) value);
				}
				break;
    		case gOverlaps:
				if (value instanceof Geometry) {
					filter.addOverlaps(binding, (Geometry) value);
				}
				break;
    		case gTouches:
				if (value instanceof Geometry) {
					filter.addTouches(binding, (Geometry) value);
				}
				break;
    		case and:
			case not:
			case or:
				break;
			default:
    		}
    	}
    }
/*
	@SuppressWarnings("unchecked")
    private void add(String matrixType, 
    					String propertyName,
						Map<String, String> parameters, 
						Persistence persistence,
						PrintWriter pw)
	throws Exception {
		StringBuilder message = new StringBuilder(256);

		// Creating a new document
		if ((propertyName == null) || matrixType.equals(propertyName)) {
			AWDDomainBean bean = BindUtil.newInstance(matrixType);
			
			BeanUtils.populate(bean, parameters);

			persistence.persist(bean);
			message.append("{response:{status:0,data:");
			message.append(JSONUtil.marshall(bean));
			message.append("}}");
		}
		else { // either connecting new, or connecting existing
			String fromId = parameters.get(AWDDomainRelationship.FROM_ID_PROPERTY_NAME);
			String toId = parameters.get(AWDDomainRelationship.TO_ID_PROPERTY_NAME);
			AWDDomainBean parentBean = persistence.retrieveBean(fromId);
			if (toId == null) { // connecting new
			}
			else { // connecting existing
				AWDDomainBean childBean = persistence.retrieveBean(toId);
				List<AWDDomainRelationship<?, ?>> rels = (List<AWDDomainRelationship<?, ?>>) BindUtil.get(parentBean, propertyName);

				AWDDomainRelationship<?, ?> relationship = BindUtil.newInstance(propertyName, parentBean, childBean);
				rels.add(relationship);
				message.append("{response:{status:0,data:");
				message.append(JSONUtil.marshall(relationship));
				message.append("}}");
			}
		}
		
		pw.append(message);
	}
*/
    private static void update(Module module, 
								Query query,
								String tagId,
								boolean rowIsTagged,
								SortedMap<String, Object> parameters, 
								AbstractPersistence persistence,
								PrintWriter pw)
	throws Exception {
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Document document = module.getDocument(customer, query.getDocumentName());

		// remove read-only property parameters as they have no setters
		parameters.remove(Bean.MODULE_KEY);
		parameters.remove(Bean.DOCUMENT_KEY);
		parameters.remove(Bean.BIZ_KEY);

		// remove parameters that are not really properties
		parameters.remove("operator");
		parameters.remove("criteria");
		
		// remove parameters that are not editable
		for (QueryColumn column : query.getColumns()) {
			String columnBinding = column.getBinding();
			if (! column.isEditable()) {
				parameters.remove(columnBinding);
			}
			
			// replace association bizIds with the real object
			TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, columnBinding);
			Attribute targetAttribute = target.getAttribute();
			if (targetAttribute instanceof Association) {
				parameters.put(columnBinding,
								persistence.retrieve(module.getName(),
														((Association) targetAttribute).getDocumentName(),
														(String) parameters.get(columnBinding),
														false));
			}
		}
		
		String bizId = (String) parameters.get(Bean.DOCUMENT_ID);
		PersistentBean bean = persistence.retrieve(document, bizId, true);

		BindUtil.populateProperties(user, bean, parameters, true);
// go through the query to get the properties
		bean = persistence.save(document, bean);

		// return the updated row
		
		pw.append(returnUpdatedMessage(customer, module, document, bizId, query, tagId, rowIsTagged));
	}

	private static void tag(Customer customer,
								Module module,
								Query query, 
								String tagId,
								Map<String, Object> parameters, 
								PrintWriter pw)
	throws Exception {
		String bizId = (String) parameters.get(Bean.DOCUMENT_ID);
		TagUtil.tag(tagId, module.getName(), query.getDocumentName(), bizId);
		
		// return the updated row
		pw.append(returnTagUpdateMessage(customer,
											parameters, 
											module, 
											query.getDocumentName(),
											query,
											true));
	}

	private static void untag(Customer customer,
								Module module,
								Query query, 
								String tagId,
								Map<String, Object> parameters, 
								PrintWriter pw)
	throws Exception {
		String bizId = (String) parameters.get(Bean.DOCUMENT_ID);
		TagUtil.untag(tagId, module.getName(), query.getDocumentName(), bizId);
		
		// return the updated row
		pw.append(returnTagUpdateMessage(customer, 
											parameters,
											module,
											query.getDocumentName(),
											query,
											false));
	}

	private static String returnUpdatedMessage(Customer customer,
												Module module,
												Document document,
												String bizId,
												Query query, 
												String tagId,
												boolean rowIstagged)
	throws Exception {
		StringBuilder message = new StringBuilder(256);
		message.append("{response:{status:0,data:");
		
		Set<String> propertyNames = new TreeSet<>();
		propertyNames.add(Bean.DOCUMENT_ID);
		propertyNames.add(PersistentBean.LOCK_NAME);
		propertyNames.add(PersistentBean.TAGGED_NAME);
		propertyNames.add(PersistentBean.FLAG_COMMENT_NAME);
		for (QueryColumn column : query.getColumns()) {
			String binding = column.getBinding();
			if (binding == null) {
				propertyNames.add(column.getName());
			}
			else {
				propertyNames.add(binding);
				TargetMetaData target = Binder.getMetaDataForBinding(customer,
																		module,
																		document,
																		binding);
				if (target.getAttribute() instanceof Association) {
					StringBuilder sb = new StringBuilder(64);
					sb.append(binding).append('.').append(Bean.BIZ_KEY);
					propertyNames.add(sb.toString());
				}
			}
			propertyNames.add((binding != null) ? binding : column.getName());
		}
		
		DocumentQuery detailQuery = query.constructDocumentQuery(null, tagId);
		detailQuery.getFilter().addEquals(Bean.DOCUMENT_ID, bizId);
		
		List<Bean> beans = detailQuery.projectedResults();
		
		// reinstate whether the record is tagged or not.
		
		String json = JSONUtil.marshall(customer, beans, propertyNames);
		if (rowIstagged) {
			json = json.replace(PersistentBean.TAGGED_NAME + "\":null", PersistentBean.TAGGED_NAME + "\":true");
		}
		message.append(json);
		message.append("}}");
		
		return message.toString();
	}
	
	private static String returnTagUpdateMessage(Customer customer,
													Map<String, Object> parameters,
													Module module,
													String documentName,
													Query query,
													boolean tagging)
	throws Exception {
		StringBuilder message = new StringBuilder(256);
		message.append("{response:{status:0,data:[");

		Map<String, Object> properties = new TreeMap<>();

		Set<String> propertyNames = new TreeSet<>();
		propertyNames.add(Bean.DOCUMENT_ID);
		properties.put(Bean.DOCUMENT_ID, parameters.get(Bean.DOCUMENT_ID));
		propertyNames.add(PersistentBean.LOCK_NAME);
		properties.put(PersistentBean.LOCK_NAME, parameters.get(PersistentBean.LOCK_NAME));
		propertyNames.add(PersistentBean.TAGGED_NAME);
		properties.put(PersistentBean.TAGGED_NAME, Boolean.valueOf(tagging));
		propertyNames.add(PersistentBean.FLAG_COMMENT_NAME);
		properties.put(PersistentBean.FLAG_COMMENT_NAME, parameters.get(PersistentBean.FLAG_COMMENT_NAME));

		Document document = module.getDocument(customer, documentName);
		
		for (QueryColumn column : query.getColumns()) {
			String binding = column.getBinding();
			if (binding == null) {
				binding = column.getName();
			}
			else {
				TargetMetaData target = Binder.getMetaDataForBinding(customer,
																		module,
																		document,
																		binding);
				Attribute attribute = target.getAttribute(); 
				if (attribute instanceof Association) {
					StringBuilder sb = new StringBuilder(64);
					sb.append(binding).append('.').append(Bean.BIZ_KEY);
					String bizKeyBinding = sb.toString();
					propertyNames.add(bizKeyBinding);
					Document associationDocument = module.getDocument(customer, ((Association) attribute).getDocumentName());
					PersistentBean relatedBean = CORE.getPersistence().retrieve(associationDocument,
																					(String) parameters.get(binding),
																					false);
					properties.put(bizKeyBinding, relatedBean.getBizKey());
				}
			}
			propertyNames.add(binding);
			properties.put(binding, parameters.get(binding));
		}

		message.append(JSONUtil.marshall(customer,
											new MapBean(module.getName(),
															documentName,
															properties),
											propertyNames));
		message.append("]}}");
		
		return message.toString();
	}

	private static void remove(Module module,
	    						Query query,
								Map<String, Object> parameters, 
								AbstractPersistence persistence,
								PrintWriter pw)
	throws Exception {
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Document document = module.getDocument(customer, query.getDocumentName());
		PersistentBean bean = persistence.retrieve(document, (String) parameters.get(Bean.DOCUMENT_ID), true);

		persistence.delete(document, bean);
		pw.append("{response:{status:0}}");
	}
}