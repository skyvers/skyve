package org.skyve.impl.web.service.smartclient;

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

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.MapBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.metadata.model.document.field.ConvertableField;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.TagUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.ConversationUtil;
import org.skyve.impl.web.SortParameterImpl;
import org.skyve.web.SortParameter;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.JSON;
import org.skyve.util.Util;

import com.vividsolutions.jts.geom.Geometry;

/**
 * Service for list views.
 */
public class SmartClientListServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	
	static final String ISC_META_DATA_PREFIX = "isc_metaDataPrefix";
	static final String ISC_DATA_FORMAT = "isc_dataFormat";
	static final String OLD_VALUES = "_oldValues";
	static final String ISC_INTERNAL_GRID_COLUMN_NAME_PREFIX = "$";
	static final String ISC_JSON_PREFIX = "<SCRIPT>//'\"]]>>isc_JSONResponseStart>>";
	static final String ISC_JSON_SUFFIX = "//isc_JSONResponseEnd";

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
        response.setCharacterEncoding(Util.UTF8);
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
					AbstractWebContext webContext = ConversationUtil.getCachedConversation(webId, request, response);
					if (webContext != null) {
						if (request.getParameter(AbstractWebContext.CONTINUE_CONVERSATION) != null) {
				        	UtilImpl.LOGGER.info("USE VIEW CONVERSATION!!!!");
				            persistence = webContext.getConversation();
				            persistence.setForThread();
						}
			        }
			        // if no conversation to use, start a new one
			        if (persistence == null) {
			            persistence = AbstractPersistence.get();
			            persistence.evictAllCached();
			        }

			        persistence.begin();
			    	Principal userPrincipal = request.getUserPrincipal();
			    	User user = WebUtil.processUserPrincipalForRequest(request,
			    														(userPrincipal == null) ? null : userPrincipal.getName(),
	    																true);
					if (user == null) {
						throw new SessionEndedException();
					}
			    	persistence.setUser(user);
	
					Bean bean = WebUtil.getConversationBeanFromRequest(webContext, request);
			    	Customer customer = user.getCustomer();
			        Module module = null;
			        Document drivingDocument = null;
			        ListModel<Bean> model = null;
					MetaDataQueryDefinition query = null;
					
			        if (dataSource != null) {
			        	// '_' split could be 2, 3 or 4 tokens
			        	// 2 - module_query, module_document (default query)
			        	// >3 - module_query_attribute (picklist), module_document_attribute (default query picklist)
			        	// '__' - module_document__model
			        	String[] tokens = dataSource.split("_");
						module = customer.getModule(tokens[0]);
						
						// model type of request
						if (dataSource.contains("__")) {
							drivingDocument = module.getDocument(customer, tokens[1]);
							model = CORE.getRepository().getListModel(customer, drivingDocument, tokens[3], true);
							if (model == null) {
								throw new ServletException("DataSource does not reference a valid model " + tokens[3]);
							}
							model.setBean(bean);
							drivingDocument = model.getDrivingDocument();
						}
						// query type of request
						else {
							String documentOrQueryName = tokens[1];
							query = module.getMetaDataQuery(documentOrQueryName);
							// not a query, must be a document
							if (query == null) {
								query = module.getDocumentDefaultQuery(customer, documentOrQueryName);
							}
							if (query == null) {
								throw new ServletException("DataSource does not reference a valid query " + documentOrQueryName);
							}
					        DocumentQueryListModel<Bean> queryModel = new DocumentQueryListModel<>();
					        queryModel.setQuery(query);
					        model = queryModel;
					        drivingDocument = module.getDocument(customer, query.getDocumentName());
						}
			        }
			        else {
			        	throw new ServletException("No datasource name in the request.");
			        }
			        
			        SortedMap<String, Object> parameters = new TreeMap<>();
					java.util.Enumeration<String> names = request.getParameterNames();
					while (names.hasMoreElements()) {
						String name = names.nextElement();
						if (ISC_META_DATA_PREFIX.equals(name) || 
								ISC_DATA_FORMAT.equals(name) ||
								// $ is sent down when list grid is in expander mode with hidden columns
								// and a record flag is cleared
								name.startsWith(ISC_INTERNAL_GRID_COLUMN_NAME_PREFIX) ||
								// don't need to add the criteria parameter to the internal parameters map
								// as these are processed specifically by fetch...
								name.equals("criteria")) {
							continue;
						}
						String value = request.getParameter(name);
						if ((! name.isEmpty()) && name.charAt(0) != '_') {
							// no '.' allowed in smart client field names
							name = BindUtil.unsanitiseBinding(name);
							
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

					String tagId = request.getParameter("_tagId");
					// "null" can be sent by Smart Client
					if (tagId != null) {
						if ((tagId.length() == 0) || "null".equals(tagId)) {
							tagId = null;
						}
					}

					switch (operation) {
					case fetch:
						if (! user.canReadDocument(drivingDocument)) {
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
								
								SortParameter sortParameter = new SortParameterImpl();
								if (sortBy.startsWith("-")) {
									sortParameter.setDirection(SortDirection.descending);
									sortBy = sortBy.substring(1);
								}
								else {
									sortParameter.setDirection(SortDirection.ascending);
								}
								sortParameter.setBy(BindUtil.unsanitiseBinding(sortBy));
								
								sortParameters[i] = sortParameter;
							}
						}
	
						String summary = request.getParameter("_summary");
						if ("".equals(summary)) {
							summary = null;
						}
						fetch(module,
								drivingDocument,
								startRow, 
								endRow, 
								operator, 
								request.getParameterValues("criteria"),
								sortParameters,
								(summary == null) ? null : AggregateFunction.valueOf(summary),
								// include a summary extra row (for list grids)
								request.getParameterMap().containsKey("_summary"),
								tagId,
								parameters, 
								persistence, 
								pw,
								model);
						break;
					case add:
						if (! user.canCreateDocument(drivingDocument)) {
							throw new SecurityException("create this data", user.getName());
						}
						break;
					case update:
						String bizTagged = (String) parameters.get(PersistentBean.TAGGED_NAME);
						if ("TAG".equals(bizTagged)) {
							tag(customer, module, model, tagId, parameters, pw);
						}
						else if ("UNTAG".equals(bizTagged)) {
							untag(customer, module, model, tagId, parameters, pw);
						}
						else {
							if (! user.canUpdateDocument(drivingDocument)) {
								throw new SecurityException("update this data", user.getName());
							}
		
							boolean rowIsTagged = false;
							String oldValuesJSON = request.getParameter(OLD_VALUES);
							if (oldValuesJSON != null) {
								rowIsTagged = oldValuesJSON.contains(PersistentBean.TAGGED_NAME + "\":true");
							}
							update(module, 
									model, 
									rowIsTagged,
									parameters, 
									persistence, 
									pw);
						}
						break;
					case remove:
						if (! user.canDeleteDocument(drivingDocument)) {
							throw new SecurityException("delete this data", user.getName());
						}
	
						remove(model, parameters, pw);
						break;
					default:
					}
					
					// serialize and cache conversation, if applicable
			    	if (webContext != null) {
			    		ConversationUtil.cacheConversation(webContext);
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
								PrintWriter pw,
								ListModel<Bean> model)
	throws Exception {
		StringBuilder message = new StringBuilder(256);

		User user = persistence.getUser();
		Customer customer = user.getCustomer();

		model.setStartRow(startRow);
		model.setEndRow(endRow);
		model.setSortParameters(sortParameters);
		model.setSummary(summaryType);
		model.setSelectedTagId(tagId);
		
		// Add filter criteria to query
		addFilterCriteriaToQuery(module, queryDocument, user, operator, criteria, parameters, tagId, model);

		Page page = model.fetch();
		List<Bean> beans = page.getRows();
		Bean summaryBean = page.getSummary();
		if (includeExtraSummaryRow) {
			BindUtil.set(summaryBean, PersistentBean.FLAG_COMMENT_NAME, summaryType);
			beans.add(summaryBean);
		}
		long totalRows = page.getTotalRows();
		if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info(String.format("totalRows = %d, row size = %d", 
																		Long.valueOf(page.getTotalRows()), 
																		Integer.valueOf(page.getRows().size())));
		Set<String> projections = model.getProjections();

		message.append("{\"response\":{");
		message.append("\"status\":0,");
		// If SmartClient requests a start row > what we have in the set
		// (maybe a criteria has constrained the set such that a page we were at doesn't exist any more)
		// then just send back a start row of 0.
		message.append("\"startRow\":").append((startRow > totalRows) ? 0 : startRow);
		message.append(",\"endRow\":");
		message.append(Math.min(totalRows, endRow));
		message.append(",\"totalRows\":");
		message.append(totalRows);
		message.append(",\"data\":");
		message.append(JSON.marshall(customer, beans, projections));
		message.append("}}");
		pw.append(message);
    }
    
	private static void addFilterCriteriaToQuery(Module module,
													Document document,
		    										User user,
		    										SmartClientFilterOperator filterOperator,
		    										String[] criteria,
		    										SortedMap<String, Object> parameters,
		    										String tagId,
													ListModel<Bean> model) 
    throws Exception {
    	SortedMap<String, Object> mutableParameters = new TreeMap<>(parameters);
    	CompoundFilterOperator compoundFilterOperator = CompoundFilterOperator.and;
    	String operatorParameter = (String) mutableParameters.get("operator");
    	if (operatorParameter != null) { // advanced criteria
    		try {
    			compoundFilterOperator = CompoundFilterOperator.valueOf(operatorParameter);
    		}
    		catch (@SuppressWarnings("unused") Exception e) {
    			// NB Smart Client sometimes sends extraneous half-arsed requests 
    			// through the advanced filter builder with a compound filter parameter of 'equals'.
    			// Ignore these and leave compound filter criteria set to 'and'.
    		}
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
					Map<String, Object> criterium = (Map<String, Object>) JSON.unmarshall(user, jsonCriteria);
    				advancedCriteria.add(criterium);
        		}
    		}
    		addAdvancedFilterCriteriaToQuery(module,
    											document,
    											user,
    											compoundFilterOperator,
    											advancedCriteria,
    											tagId,
												model);
    		
    		mutableParameters.remove("operator");
    		mutableParameters.remove("criteria");
    	}

    	// simple criteria or extra criteria from grid filter parameters
    	addSimpleFilterCriteriaToQuery(module,
										document,
										user.getCustomer(),
										filterOperator,
										mutableParameters,
										tagId,
										model);
    }

    /**
     * Add simple criteria to the query.
     * 
     * @param module
     * @param document
     * @param customer
     * @param query	Add the filter critiera to this query.
     * @param filterOperator	The default operator to use for all filter critiera.
     * @param criteria	A Map of name value pairs
     * @throws Exception
     */
    public static void addSimpleFilterCriteriaToQuery(Module module,
														Document document,
														Customer customer,
			    										SmartClientFilterOperator filterOperator,
			    										Map<String, Object> criteria,
			    										String tagId,
		    											ListModel<Bean> model) 
    throws Exception {
    	Filter filter = model.getFilter();
    	
    	// This doesn't need to set up a sub-filter as its just ANDing criteria
    	// and the other criteria already added should have taken care of bracketing
    	// for correct operator precedence.
    	for (String binding : criteria.keySet()) {
			Object value = criteria.get(binding);
			if ("".equals(value)) {
				value = null;
			}
			
			binding = BindUtil.unsanitiseBinding(binding);
			boolean parameter = (binding.charAt(0) == ':');
			if (parameter) {
				binding = binding.substring(1); // lose the colon
			}
			
			// Determine the type and converter of the filtered attribute
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
    			if (! parameter) {
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
						equalsOperatorRequired = true;
						if (! parameter) {
							type = String.class;
							binding = String.format("%s.%s", binding, Bean.DOCUMENT_ID);
						}
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

			if (parameter) {
				model.putParameter(binding, value);
			}
			else {
				equalsOperatorRequired = equalsOperatorRequired || 
											(value instanceof Date) ||
											(value instanceof Number) ||
											(value instanceof Boolean);
				addCriteriumToFilter(binding, 
										equalsOperatorRequired ? SmartClientFilterOperator.equals : filterOperator, 
										value, 
										null, 
										null,
										tagId,
										filter);
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
															CompoundFilterOperator compoundFilterOperator,
															List<Map<String, Object>> criteria,
															String tagId,
															ListModel<?> model)
	throws Exception {
		// We have to unconditionally add a new filter here as the metadata query
		// might have a filter stanza in it which we can't detect and we need to ensure
		// any filtering done here is AND'd with any existing criteria (hard-coded in a filter stanza or not)
		Filter filter = model.getFilter();
		Filter newFilter = model.newFilter();
		addAdvancedFilterCriteriaToQueryInternal(module,
													document,
													user,
													compoundFilterOperator,
													criteria,
													tagId,
													model,
													newFilter);
		if (! newFilter.isEmpty()) {
			filter.addAnd(newFilter);
		}
	}
    
	private static final String HIERARCHICAL_PARENT_ID_SUFFIX = "." + HierarchicalBean.PARENT_ID;

	private static void addAdvancedFilterCriteriaToQueryInternal(Module module,
																	Document document,
																	User user,
						    										CompoundFilterOperator compoundFilterOperator,
						    										List<Map<String, Object>> criteria,
						    										String tagId,
																	ListModel<?> model,
																	Filter filter)
    throws Exception {
		if (criteria != null) {
			boolean firstCriteriaIteration = true; // the first filter criteria encountered - not a bound parameter
			for (Map<String, Object> criterium : criteria) {
				if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info("criterium = " + JSON.marshall(null, criterium, null));
				String binding = ((String) criterium.get("fieldName"));
				binding = BindUtil.unsanitiseBinding(binding);
				SmartClientFilterOperator filterOperator = SmartClientFilterOperator.valueOf((String) criterium.get("operator"));

				if (binding == null) { // advanced criteria
					Filter subFilter = model.newFilter();
					CompoundFilterOperator subCompoundFilterOperator = CompoundFilterOperator.valueOf(filterOperator.toString());
					@SuppressWarnings("unchecked")
					List<Map<String, Object>> subCritiera = (List<Map<String, Object>>) criterium.get("criteria");
					addAdvancedFilterCriteriaToQueryInternal(module,
																document,
																user,
																subCompoundFilterOperator,
																subCritiera,
																tagId,
																model,
																subFilter);
					if (! subFilter.isEmpty()) {
						if (CompoundFilterOperator.or.equals(compoundFilterOperator)) {
							filter.addOr(subFilter);
						}
						else { // not is taken into account below
							filter.addAnd(subFilter);
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
	
		    		boolean parameter = (binding.charAt(0) == ':');
		    		if (parameter) {
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
		    			if (! parameter) {
		    				throw e;
		    			}
		    		}
		    		if (target != null) {
	    				Attribute attribute = target.getAttribute();
	    				if (attribute != null) {
							if (attribute instanceof Enumeration) {
								type = AbstractRepository.get().getEnum((Enumeration) attribute);
								filterOperator = transformWildcardFilterOperator(filterOperator);
							}
							else {
								type = attribute.getAttributeType().getImplementingType();
							}
							if (attribute.getDomainType() != null) {
								filterOperator = transformWildcardFilterOperator(filterOperator);
							}
	    					if (attribute instanceof ConvertableField) {
	    						ConvertableField field = (ConvertableField) attribute;
	    						converter = field.getConverterForCustomer(customer);
	    					}
	    					else if (attribute instanceof Association) {
	    						type = String.class;
	    	    				binding = new StringBuilder(binding.length() + 6).append(binding).append('.').append(Bean.DOCUMENT_ID).toString();
								filterOperator = transformWildcardFilterOperator(filterOperator);
	    					}
	    				}
		    			else if (ChildBean.PARENT_NAME.equals(binding) || binding.endsWith(ListModel.CHILD_PARENT_NAME_SUFFIX)) {
		    				type = String.class;
		    				binding = new StringBuilder(binding.length() + 6).append(binding).append('.').append(Bean.DOCUMENT_ID).toString();
		    			}
		    			else if (HierarchicalBean.PARENT_ID.equals(binding) || binding.endsWith(HIERARCHICAL_PARENT_ID_SUFFIX)) {
		    				type = String.class;
		    			}
	    			}
	    			
	    			value = fromString(binding, "value", valueString, customer, converter, type);
	
	    			if (parameter) {
	    				model.putParameter(binding, value);
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

	    			if ((value instanceof Date) || (value instanceof Number) || (value instanceof Boolean) ||
	    					(start instanceof Date) || (start instanceof Number) || (start instanceof Boolean) ||
	    					(end instanceof Date) || (end instanceof Number) || (end instanceof Boolean)) {
						filterOperator = transformWildcardFilterOperator(filterOperator);
	    			}
					
		    		switch (compoundFilterOperator) {
		    		case and:
		    			addCriteriumToFilter(binding, filterOperator, value, start, end, tagId, filter);
		    			break;
		    		case or:
		    			if (firstCriteriaIteration) {
		    				addCriteriumToFilter(binding, filterOperator, value, start, end, tagId, filter);
		    			}
		    			else {
		    				Filter orFilter = model.newFilter();
			    			addCriteriumToFilter(binding, filterOperator, value, start, end, tagId, orFilter);
			    			if (! orFilter.isEmpty()) {
			    				filter.addOr(orFilter);
			    			}
		    			}
		    			break;
		    		case not:
		    			switch (filterOperator) {
		    			case substring:
		    			case iContains:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.iNotContains, value, start, end, tagId, filter);
		    				break;
		    			case iNotContains:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.iContains, value, start, end, tagId, filter);
		    				break;
		    			case startsWith:
		    			case iStartsWith:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.iNotStartsWith, value, start, end, tagId, filter);
		    				break;
		    			case iNotStartsWith:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.iStartsWith, value, start, end, tagId, filter);
		    				break;
		    			case iEndsWith:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.iNotEndsWith, value, start, end, tagId, filter);
		    				break;
		    			case iNotEndsWith:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.iEndsWith, value, start, end, tagId, filter);
		    				break;
		    			case exact:
		    			case equals:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.notEqual, value, start, end, tagId, filter);
		    				break;
		    			case iEquals:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.iNotEqual, value, start, end, tagId, filter);
		    				break;
		    			case notEqual:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.equals, value, start, end, tagId, filter);
		    				break;
		    			case iNotEqual:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.iEquals, value, start, end, tagId, filter);
		    				break;
		    			case greaterThan:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.lessOrEqual, value, start, end, tagId, filter);
		    				break;
		    			case greaterOrEqual:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.lessThan, value, start, end, tagId, filter);
		    				break;
		    			case lessThan:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.greaterOrEqual, value, start, end, tagId, filter);
		    				break;
		    			case lessOrEqual:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.greaterThan, value, start, end, tagId, filter);
		    				break;
		    			case betweenInclusive:
		    			case iBetweenInclusive:
		    				if (start != null) {
				    			addCriteriumToFilter(binding, SmartClientFilterOperator.lessOrEqual, start, null, null, tagId, filter);
		    				}
		    				if (end != null) {
				    			addCriteriumToFilter(binding, SmartClientFilterOperator.greaterThan, end, null, null, tagId, filter);
		    				}
		    				break;
		    			case isNull:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.notNull, value, start, end, tagId, filter);
		    				break;
		    			case notNull:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.isNull, value, start, end, tagId, filter);
		    				break;
		    			case equalsField:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.notEqualField, value, start, end, tagId, filter);
		    				break;
		    			case notEqualField:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.equalsField, value, start, end, tagId, filter);
		    				break;
		    			case greaterThanField:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.lessOrEqualField, value, start, end, tagId, filter);
		    				break;
		    			case greaterOrEqualField:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.lessThanField, value, start, end, tagId, filter);
		    				break;
		    			case lessThanField:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.greaterOrEqualField, value, start, end, tagId, filter);
		    				break;
		    			case lessOrEqualField:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.greaterThanField, value, start, end, tagId, filter);
		    				break;
						case inSet:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.notInSet, value, start, end, tagId, filter);
		    				break;
						case notInSet:
			    			addCriteriumToFilter(binding, SmartClientFilterOperator.inSet, value, start, end, tagId, filter);
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
    									Class<?> type) {
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
    
    private static SmartClientFilterOperator transformWildcardFilterOperator(SmartClientFilterOperator filterOperator) {
    	if (SmartClientFilterOperator.iContains.equals(filterOperator) ||
    			SmartClientFilterOperator.iEndsWith.equals(filterOperator) ||
    			SmartClientFilterOperator.iEquals.equals(filterOperator) ||
    			SmartClientFilterOperator.iregexp.equals(filterOperator) ||
    			SmartClientFilterOperator.iStartsWith.equals(filterOperator) ||
    			SmartClientFilterOperator.regexp.equals(filterOperator) ||
    			SmartClientFilterOperator.startsWith.equals(filterOperator) ||
    			SmartClientFilterOperator.substring.equals(filterOperator)) {
    		return SmartClientFilterOperator.equals;
    	}
    	
    	if (SmartClientFilterOperator.iNotContains.equals(filterOperator) ||
    			SmartClientFilterOperator.iNotEndsWith.equals(filterOperator) ||
    			SmartClientFilterOperator.iNotEqual.equals(filterOperator) ||
    			SmartClientFilterOperator.iNotStartsWith.equals(filterOperator)) {
    		return SmartClientFilterOperator.notEqual;
    	}
    	
    	return filterOperator;
    }

    private static void addCriteriumToFilter(String binding,
    											SmartClientFilterOperator filterOperator, 
    											Object value,
    											Object start,
    											Object end,
    											String tagId,
    											Filter filter) {
    	if (PersistentBean.TAGGED_NAME.equals(binding)) {
			if ("true".equals(value)) {
				filter.addTagged(tagId, true);
			}
    		else if ("false".equals(value)) {
    			filter.addTagged(tagId, false);
    		}
    	}
    	else if (filterOperator == null) {
    		if (HierarchicalBean.PARENT_ID.equals(binding)) {
    			String parentId = (String) value;
    			if (value == null) { // get all root nodes in the data
    				filter.addNull(binding);
    			}
    			else if (parentId.startsWith("_")) { // get a particular root node
    				filter.addEquals(Bean.DOCUMENT_ID, parentId.substring(1));
    			}
    			else { // traversing the tree
    				filter.addEquals(binding, parentId);
    			}
    		}
    		else if (value != null) {
    			ListModel.addEquals(filter, binding, value);
    		}
    	}
    	else {
    		// Use the code string for enums and others for the string operators (contains etc), 
    		// otherwise hibernate will moan
    		Object revisedValue = value;
    		if (value instanceof org.skyve.domain.types.Enumeration) {
    			revisedValue = ((org.skyve.domain.types.Enumeration) value).toCode();
    		}

    		if (HierarchicalBean.PARENT_ID.equals(binding)) {
    			String parentId = (String) revisedValue;
    			if (parentId == null) { // get all root nodes in the data
    				filter.addNull(binding);
    			}
    			else if (parentId.startsWith("_")) { // get a particular root node
    				filter.addEquals(Bean.DOCUMENT_ID, parentId.substring(1));
    			}
    			else { // traversing the tree
    				filter.addEquals(binding, parentId);
    			}
    		}
    		else {
	    		switch (filterOperator) {
	    		case substring:
	    		case iContains:
	    			if (revisedValue != null) {
	    				filter.addContains(binding, revisedValue.toString());
	    			}
	    			break;
	    		case iNotContains:
	    			if (revisedValue != null) {
	    				filter.addNotContains(binding, revisedValue.toString());
	    			}
	    			break;
	    		case startsWith:
	    		case iStartsWith:
	    			if (revisedValue != null) {
	    				filter.addStartsWith(binding, revisedValue.toString());
	    			}
	    			break;
	    		case iNotStartsWith:
	    			if (revisedValue != null) {
	    				filter.addNotStartsWith(binding,  revisedValue.toString());
	    			}
	    			break;
	    		case iEndsWith:
	    			if (revisedValue != null) {
	    				filter.addEndsWith(binding, revisedValue.toString());
	    			}
	    			break;
	    		case iNotEndsWith:
	    			if (revisedValue != null) {
	    				filter.addNotEndsWith(binding, revisedValue.toString());
	    			}
	    			break;
	    		case equals:
	    		case exact:
	    			if (value != null) {
	    				ListModel.addEquals(filter, binding, value);
	    			}
	    			break;
	    		case iEquals:
	    			if (revisedValue != null) {
	    				filter.addEqualsIgnoreCase(binding, revisedValue.toString());
	    			}
	    			break;
	    		case notEqual:
	    			if (value != null) {
	    				ListModel.addNotEquals(filter, binding, value);
	    			}
	    			break;
	    		case iNotEqual:
	    			if (revisedValue != null) {
	    				filter.addNotEqualsIgnoreCase(binding, revisedValue.toString());
	    			}
	    			break;
	    		case greaterThan:
	    			if (value != null) {
	    				ListModel.addGreaterThan(filter, binding, value);
	    			}
	    			break;
	    		case greaterOrEqual:
	    			if (value != null) {
	    				ListModel.addGreaterThanOrEqualTo(filter, binding, value);
	    			}
	    			break;
	    		case lessThan:
	    			if (value != null) {
	    				ListModel.addLessThan(filter, binding, value);
	    			}
	    			break;
	    		case lessOrEqual:
	    			if (value != null) {
	    				ListModel.addLessThanOrEqualTo(filter, binding, value);
	    			}
	    			break;
	    		case iBetweenInclusive:
	    		case betweenInclusive:
	    			if ((start != null) && (end != null)) {
	    				ListModel.addBetween(filter, binding, start, end);
	    			}
	    			else if (start != null) {
	    				ListModel.addGreaterThanOrEqualTo(filter, binding, start);
	    			}
	    			else if (end != null) {
	    				ListModel.addLessThanOrEqualTo(filter, binding, end);
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
			message.append("{\"response\":{\"status\":0,\"data\":");
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
				message.append("{\"response\":{\"status\":0,\"data\":");
				message.append(JSONUtil.marshall(relationship));
				message.append("}}");
			}
		}
		
		pw.append(message);
	}
*/
    private static void update(Module module, 
								ListModel<Bean> model,
								boolean rowIsTagged,
								SortedMap<String, Object> properties, 
								AbstractPersistence persistence,
								PrintWriter pw)
	throws Exception {
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Document document = model.getDrivingDocument();
		
		// remove read-only property parameters as they have no setters
		properties.remove(Bean.MODULE_KEY);
		properties.remove(Bean.DOCUMENT_KEY);
		properties.remove(Bean.BIZ_KEY);

		// remove parameters that are not really properties
		properties.remove("operator");
		properties.remove("criteria");
		
		// remove parameters that are not editable
		for (MetaDataQueryColumn column : model.getColumns()) {
			String columnBinding = column.getBinding();
			if (columnBinding == null) {
				columnBinding = column.getName();
			}
			
			if ((! (column instanceof MetaDataQueryProjectedColumn)) ||
					(! ((MetaDataQueryProjectedColumn) column).isEditable())) {
				properties.remove(columnBinding);
			}
			else {
				// replace association bizIds with the real object
				TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, columnBinding);
				Attribute targetAttribute = target.getAttribute();
				if (targetAttribute instanceof Association) {
					String associationId = (String) properties.get(columnBinding);
					if (associationId == null) {
						properties.put(columnBinding, null);
					}
					else {
						properties.put(columnBinding,
										persistence.retrieve(module.getName(),
																((Association) targetAttribute).getDocumentName(),
																associationId,
																false));
					}
				}
			}
		}
		
		String bizId = (String) properties.remove(Bean.DOCUMENT_ID);
		Bean bean = model.update(bizId, properties);

		// return the updated row
		pw.append(returnUpdatedMessage(customer, model, bean, rowIsTagged));
    }

	private static void tag(Customer customer,
								Module module,
								ListModel<Bean> model, 
								String tagId,
								Map<String, Object> parameters, 
								PrintWriter pw)
	throws Exception {
		String bizId = (String) parameters.get(Bean.DOCUMENT_ID);
		TagUtil.tag(tagId, module.getName(), model.getDrivingDocument().getName(), bizId);
		
		// return the updated row
		pw.append(returnTagUpdateMessage(customer,
											parameters, 
											module, 
											model,
											true));
	}

	private static void untag(Customer customer,
								Module module,
								ListModel<Bean> model,
								String tagId,
								Map<String, Object> parameters, 
								PrintWriter pw)
	throws Exception {
		String bizId = (String) parameters.get(Bean.DOCUMENT_ID);
		TagUtil.untag(tagId, module.getName(), model.getDrivingDocument().getName(), bizId);
		
		// return the updated row
		pw.append(returnTagUpdateMessage(customer, 
											parameters,
											module,
											model,
											false));
	}

	private static String returnUpdatedMessage(Customer customer,
												ListModel<Bean> model, 
												Bean bean,
												boolean rowIstagged)
	throws Exception {
		StringBuilder message = new StringBuilder(256);
		message.append("{\"response\":{\"status\":0,\"data\":");

		// reinstate whether the record is tagged or not.
		String json = JSON.marshall(customer, bean, model.getProjections());
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
													ListModel<Bean> model,
													boolean tagging)
	throws Exception {
		StringBuilder message = new StringBuilder(256);
		message.append("{\"response\":{\"status\":0,\"data\":[");

		Set<String> projections = model.getProjections();
		Map<String, Object> properties = new TreeMap<>();
		for (String projection : projections) {
			if (PersistentBean.TAGGED_NAME.equals(projection)) {
				properties.put(projection, Boolean.valueOf(tagging));
			}
			else {
				properties.put(projection, parameters.get(projection));
			}
		}

		message.append(JSON.marshall(customer,
											new MapBean(module.getName(),
															model.getDrivingDocument().getName(),
															properties),
											projections));
		message.append("]}}");
		
		return message.toString();
	}

	private static void remove(ListModel<Bean> model,
								Map<String, Object> parameters, 
								PrintWriter pw)
	throws Exception {
		model.remove((String) parameters.get(Bean.DOCUMENT_ID));
		pw.append("{\"response\":{\"status\":0}}");
	}
}