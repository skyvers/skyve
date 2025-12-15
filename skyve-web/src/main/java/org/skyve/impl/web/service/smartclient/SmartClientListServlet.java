package org.skyve.impl.web.service.smartclient;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.locationtech.jts.geom.Geometry;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.TransientBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.NoResultsException;
import org.skyve.domain.messages.SecurityException;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.metadata.model.document.field.ConvertibleField;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.snapshot.CompoundFilterOperator;
import org.skyve.impl.snapshot.SmartClientFilterOperator;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.SortParameterImpl;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.JSON;
import org.skyve.util.OWASP;
import org.skyve.util.Util;
import org.skyve.util.logging.Category;
import org.skyve.util.monitoring.Monitoring;
import org.skyve.util.monitoring.RequestKey;
import org.skyve.web.SortParameter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

/**
 * Service for list views.
 */
public class SmartClientListServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = LoggerFactory.getLogger(SmartClientListServlet.class);
	private static final Logger COMMAND_LOGGER = Category.COMMAND.logger();

	static final String ISC_META_DATA_PREFIX = "isc_metaDataPrefix";
	static final String ISC_DATA_FORMAT = "isc_dataFormat";
	static final String OLD_VALUES = "_oldValues";
	static final String ISC_INTERNAL_GRID_COLUMN_NAME_PREFIX = "$";
	static final String ISC_JSON_PREFIX = "<SCRIPT>//'\"]]>>isc_JSONResponseStart>>";
	static final String ISC_JSON_SUFFIX = "//isc_JSONResponseEnd";

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		LOGGER.info("SmartClientList - get....");
		processRequest(request, response);
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		LOGGER.info("SmartClientList - post....");
		processRequest(request, response);
	}

	private static void processRequest(HttpServletRequest request, HttpServletResponse response)
	throws IOException {
		response.setContentType(MimeType.json.toString());
		response.setCharacterEncoding(Util.UTF8);
		response.addHeader("Cache-control", "private,no-cache,no-store"); // never
		response.addDateHeader("Expires", 0); // never

		// Send CSRF Token as a response header (must be done before getting the writer)
		String currentCsrfTokenString = UtilImpl.processStringValue(request.getParameter(AbstractWebContext.CSRF_TOKEN_NAME));
		Integer currentCsrfToken = (currentCsrfTokenString == null) ? null : Integer.valueOf(currentCsrfTokenString);
		Integer newCsrfToken = currentCsrfToken;
		String operationType = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("_operationType")));
		// If this is a mutating request, we'll definitely need a new CSRF Token
		if (Operation.fetch.toString().equals(operationType)) {
			if (newCsrfToken == null) {
				newCsrfToken = StateUtil.createToken();
			}
		}
		else {
			newCsrfToken = StateUtil.createToken();
		}
		response.setIntHeader("X-CSRF-TOKEN", newCsrfToken.intValue());

		try (PrintWriter pw = response.getWriter()) {
			String dataSource = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("_dataSource")));
			if (operationType == null) {
				pw.append("{}");
				return;
			}
			Operation operation = Operation.valueOf(operationType);
			AbstractPersistence persistence = null;

			try {
				try {
					// use the view's conversation if it was sent down from the client
					String webId = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter(AbstractWebContext.CONTEXT_NAME)));
					AbstractWebContext webContext = StateUtil.getCachedConversation(webId, request);
					if ((webContext != null) &&
							(request.getParameter(AbstractWebContext.CONTINUE_CONVERSATION) != null)) {
						LOGGER.info("USE VIEW CONVERSATION!!!!");
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
					User user = WebUtil.processUserPrincipalForRequest(request, (userPrincipal == null) ? null : userPrincipal.getName());
					if (user == null) {
						throw new SessionEndedException(request.getLocale());
					}
					persistence.setUser(user);

					Bean bean = WebUtil.getConversationBeanFromRequest(webContext, request);
					Customer customer = user.getCustomer();
					Module module = null;
					Document drivingDocument = null;
					RequestKey key = null;
					ListModel<Bean> model = null;
					MetaDataQueryDefinition query = null;

					if (dataSource != null) {
						// '_' split could be 2, 3 or 4 tokens
						// 2 - module_query, module_document (default query)
						// >3 - module_query_attribute (picklist), module_document_attribute (default query picklist)
						// '__' - module_document__model
						String[] tokens = dataSource.split("_");
						String moduleName = tokens[0];
						module = customer.getModule(moduleName);

						UxUi uxui = UserAgent.getUxUi(request);
						// model type of request
						if (dataSource.contains("__")) {
							final String documentName = tokens[1];
							final String modelName = tokens[3];
							EXT.checkAccess(user, UserAccess.modelAggregate(moduleName, documentName, modelName), uxui.getName());

							drivingDocument = module.getDocument(customer, documentName);
							model = drivingDocument.getListModel(customer, modelName, true);
							if (model == null) {
								throw new ServletException("DataSource does not reference a valid model " + tokens[3]);
							}
							key = RequestKey.model(drivingDocument, modelName);
							model.setBean(bean);
							drivingDocument = model.getDrivingDocument();
						}
						// query type of request
						else {
							final String documentOrQueryName = tokens[1];
							query = module.getMetaDataQuery(documentOrQueryName);
							// not a query, must be a document
							if (query == null) {
								EXT.checkAccess(user, UserAccess.documentAggregate(moduleName, documentOrQueryName), uxui.getName());
								query = module.getDocumentDefaultQuery(customer, documentOrQueryName);
								key = RequestKey.documentListModel(moduleName, documentOrQueryName);
							}
							else {
								EXT.checkAccess(user, UserAccess.queryAggregate(moduleName, documentOrQueryName), uxui.getName());
								key = RequestKey.queryListModel(moduleName, documentOrQueryName);
							}
							model = EXT.newListModel(query);
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
						String[] values = request.getParameterValues(name);
						if ((! name.isEmpty()) && name.charAt(0) != '_') {
							// no '.' allowed in smart client field names
							name = BindUtil.unsanitiseBinding(name);

							if (values == null) {
								parameters.put(name, null);
							}
							else {
								for (int i = 0, l = values.length; i < l; i++) {
									String value = values[i];
									if (value != null) {
										// "null" can be sent by Smart Client
										if (value.isBlank() || "null".equals(value)) {
											values[i] = null;
										}
										else {
											values[i] = OWASP.unescapeHtmlChars(value);
										}
									}
								}

								Object value = (values.length == 1) ? values[0] : Arrays.copyOf(values, values.length, Object[].class);
								parameters.put(name, value);
							}
						}
					}
					for (String name : parameters.keySet()) {
						LOGGER.info("{} = {}", name, parameters.get(name));
					}

					String tagId = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("_tagId")));
					// "null" can be sent by Smart Client
					if ((tagId != null) && (tagId.isEmpty() || "null".equals(tagId))) {
						tagId = null;
					}

					HttpSession session = request.getSession();

					switch (operation) {
						case fetch:
							if (! user.canReadDocument(drivingDocument)) {
								throw new SecurityException("read this data", user.getName());
							}
							String _startRow = Util.processStringValue(request.getParameter("_startRow"));
							int startRow = (_startRow == null) ? 0 : Integer.parseInt(_startRow);
							String _endRow = Util.processStringValue(request.getParameter("_endRow"));
							int endRow = (_endRow == null) ? Integer.MAX_VALUE : Integer.parseInt(_endRow);
							String textMatchStyle = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("_textMatchStyle")));
							SmartClientFilterOperator operator = null;
							if (textMatchStyle != null) {
								operator = SmartClientFilterOperator.valueOf(textMatchStyle);
							}
	
							String[] sortBys = request.getParameterValues("_sortBy");
							SortParameter[] sortParameters = null;
							if (sortBys != null) {
								sortParameters = new SortParameter[sortBys.length];
								for (int i = 0; i < sortBys.length; i++) {
									String sortBy = OWASP.sanitise(Sanitisation.text, Util.processStringValue(sortBys[i]));
	
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
	
							String summary = OWASP.sanitise(Sanitisation.text, Util.processStringValue(request.getParameter("_summary")));
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
							checkCsrfToken(session, request, response, currentCsrfToken);
	
							if (! user.canCreateDocument(drivingDocument)) {
								throw new SecurityException("create this data", user.getName());
							}
							break;
						case update:
							checkCsrfToken(session, request, response, currentCsrfToken);
	
							String bizTagged = (String) parameters.get(PersistentBean.TAGGED_NAME);
							String bizFlagComment = request.getParameter(PersistentBean.FLAG_COMMENT_NAME);
							if ("TAG".equals(bizTagged)) {
								tag(user, customer, module, model, tagId, parameters, pw);
							}
							else if ("UNTAG".equals(bizTagged)) {
								untag(user, customer, module, model, tagId, parameters, pw);
							}
							else if (bizFlagComment != null) {
								bizFlagComment = OWASP.sanitise(Sanitisation.basic, Util.processStringValue(bizFlagComment));
	
								if (! user.canUpdateDocument(drivingDocument)) {
									throw new SecurityException("update this data", user.getName());
								}
								if (! user.canFlag()) {
									throw new SecurityException("flag this data", user.getName());
								}
	
								if (! drivingDocument.isPersistable()) {
									throw new ServletException("Flagging is not available");
								}
	
								flag(request,
										pw,
										persistence,
										user,
										customer,
										module,
										drivingDocument,
										model,
										parameters,
										bizFlagComment);
							}
							else {
								if (! user.canUpdateDocument(drivingDocument)) {
									throw new SecurityException("update this data", user.getName());
								}
	
								update(module,
										model,
										isRowTagged(request),
										parameters,
										persistence,
										pw);
							}
							break;
						case remove:
							checkCsrfToken(session, request, response, currentCsrfToken);
	
							if (! user.canDeleteDocument(drivingDocument)) {
								throw new SecurityException("delete this data", user.getName());
							}
	
							remove(model, parameters, pw);
							break;
						default:
					}

					// Replace CSRF token
					StateUtil.replaceToken(session, currentCsrfToken, newCsrfToken);

					// serialize and cache conversation, if applicable
					if (webContext != null) {
						StateUtil.cacheConversation(webContext);
					}
					
					if (key != null) {
						Monitoring.measure(key);
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
			beans.add(summaryBean);
		}
		long totalRows = page.getTotalRows();
		if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("totalRows = {}, row size = {}",
																		Long.valueOf(page.getTotalRows()),
																		Integer.valueOf(page.getRows().size()));

		Set<String> projections = processRows(beans, model, user, customer, module, queryDocument);

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
		Util.chunkCharsToWriter(message, pw);
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
					// Get each criterion name, operator and operands
					@SuppressWarnings("unchecked")
					Map<String, Object> criterion = (Map<String, Object>) JSON.unmarshall(user, jsonCriteria);

					// Check for filter by flag permissions
					if (PersistentBean.FLAG_COMMENT_NAME.equals(criterion.get("fieldName"))) {
						if (! user.canFlag()) {
							throw new SecurityException("filter by flag", user.getName());
						}
					}

					advancedCriteria.add(criterion);
				}
			}
			addAdvancedFilterCriteriaToQuery(module, document, user, compoundFilterOperator, advancedCriteria, tagId, model);

			mutableParameters.remove("operator");
			mutableParameters.remove("criteria");
		}

		// check for filter by flag permissions
		if (mutableParameters.containsKey(PersistentBean.FLAG_COMMENT_NAME) && (! user.canFlag())) {
			throw new SecurityException("filter by flag", user.getName());
		}

		// simple criteria or extra criteria from grid filter parameters
		addSimpleFilterCriteriaToQuery(module, document, user.getCustomer(), filterOperator, mutableParameters, tagId, model);
	}

	// Add display values and sanitise
	// Returns the projections required from JSON.marshall()
	private static Set<String> processRows(List<Bean> beans,
											ListModel<Bean> model,
											User user,
											Customer customer,
											Module module,
											Document document) {
		// Determine if any display bindings are required for dynamic or variant domain attributes
		// or for formats defined on the columns.
		// Map of binding to synthesized display binding for SC.
		Map<String, String> displayBindings = new TreeMap<>();
		// Map of binding to synthesized formatted display for SC.
		Map<String, Pair<String, String>> formatBindings = new TreeMap<>();
		for (MetaDataQueryColumn column : model.getColumns()) {
			String binding = column.getBinding();
			String name = column.getName();
			String key = (binding != null) ? binding : name;

			// Check for formatters on the column
			if (column instanceof MetaDataQueryProjectedColumn projectedColumn) {
				FormatterName formatterName = projectedColumn.getFormatterName();
				if (formatterName != null) {
					formatBindings.put(key, new ImmutablePair<>(formatterName.name(), "_display_" + BindUtil.sanitiseBinding(key)));
					continue;
				}
				String customFormatterName = projectedColumn.getCustomFormatterName();
				if (customFormatterName != null) {
					formatBindings.put(key, new ImmutablePair<>(customFormatterName, "_display_" + BindUtil.sanitiseBinding(key)));
					continue;
				}
			}

			// Check for variant or dynamic domain types
			if (binding != null) {
				TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
				Attribute attribute = target.getAttribute();
				if (attribute != null) {
					DomainType domainType = attribute.getDomainType();
					if ((domainType == DomainType.variant) || (domainType == DomainType.dynamic)) {
						displayBindings.put(binding, "_display_" + BindUtil.sanitiseBinding(binding));
					}
				}
			}
		}

		boolean userCantFlag = ! user.canFlag();
		boolean extraDisplayOrFormatBindings = (! displayBindings.isEmpty()) || (! formatBindings.isEmpty());

		// Defend against transient beans and incomplete dynamic beans
		// Nullify Flag Comments not accessible
		// Add the display/format bindings in if some are required
		for (int i = 0, l = beans.size(); i < l; i++) {
			// whether we should null the flag comment
			boolean nullFlagComment = userCantFlag && // the user can't flag and
										(i < (l - 1)); // we are processing a data row (not the summary row)
			Bean bean = beans.get(i);
			if (bean instanceof TransientBean) {
				Map<String, Object> properties = new TreeMap<>();
				properties.put(DocumentQuery.THIS_ALIAS, bean);
				properties.put(PersistentBean.VERSION_NAME, null);
				properties.put(PersistentBean.LOCK_NAME, null);
				properties.put(PersistentBean.TAGGED_NAME, Boolean.FALSE);
				properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
				bean = new DynamicBean(bean.getBizModule(), bean.getBizDocument(), properties);
				beans.set(i, bean);
				nullFlagComment = false; // just set this null above
			}
			else if (bean instanceof DynamicBean dynamicBean) {
				// Determine if the dynamic bean is incomplete - the bean does not wrap an AbstractPersistentBean
				boolean potentiallyIncomplete = true;
				if (dynamicBean.isDynamic(DocumentQuery.THIS_ALIAS)) {
					Bean wrappedBean = (Bean) dynamicBean.get(DocumentQuery.THIS_ALIAS);
					if (wrappedBean instanceof AbstractPersistentBean) {
						potentiallyIncomplete = false;
					}
				}
				if (potentiallyIncomplete) {
					if (! dynamicBean.isProperty(PersistentBean.VERSION_NAME)) {
						dynamicBean.putDynamic(PersistentBean.VERSION_NAME, null);
					}
					if (! dynamicBean.isProperty(PersistentBean.LOCK_NAME)) {
						dynamicBean.putDynamic(PersistentBean.LOCK_NAME, null);
					}
					if (! dynamicBean.isProperty(PersistentBean.TAGGED_NAME)) {
						dynamicBean.putDynamic(PersistentBean.TAGGED_NAME, Boolean.FALSE);
					}
					boolean missingFlagComment = (!dynamicBean.isProperty(PersistentBean.FLAG_COMMENT_NAME));
					if (missingFlagComment || nullFlagComment) {
						dynamicBean.putDynamic(PersistentBean.FLAG_COMMENT_NAME, null);
						nullFlagComment = false; // just set this null
					}
				}
			}

			// Nullify flag comments if not given permissions
			if (nullFlagComment) {
				BindUtil.set(bean, PersistentBean.FLAG_COMMENT_NAME, null);
			}

			// Add extra bindings if required
			if (extraDisplayOrFormatBindings) {
				for (Entry<String, Pair<String, String>> entry : formatBindings.entrySet()) {
					Pair<String, String> value = entry.getValue();
					String display = CORE.format(value.getLeft(), Binder.get(bean, entry.getKey()));
					bean.putDynamic(value.getRight(), display);
				}
				for (Entry<String, String> entry : displayBindings.entrySet()) {
					String display = BindUtil.getDisplay(customer, bean, entry.getKey());
					bean.putDynamic(entry.getValue(), display);
				}
			}
		}

		// Sanitise rows
		// Note that HTML escaping is taken care by SC client-side for data grid columns
		OWASP.sanitiseAndEscapeListModelRows(beans, model.getColumns(), false);

		// Setup projections from the model plus any added display/format bindings
		Set<String> result = null;
		if (extraDisplayOrFormatBindings) {
			result = new TreeSet<>(model.getProjections());
			result.addAll(displayBindings.values());
			for (Pair<String, String> value : formatBindings.values()) {
				result.add(value.getRight());
			}
		}
		else {
			result = model.getProjections();
		}
		return result;
	}

	@SuppressWarnings("unused")
	static void checkCsrfToken(HttpSession session,
								HttpServletRequest request,
								HttpServletResponse response,
								Integer currentCsrfToken) {
/* TODO Commented out until I can work out the async problems in SC
		if (! StateUtil.checkToken(session, currentCsrfToken)) {
			WebUtil.logout(request, response);
			throw new java.lang.SecurityException("CSRF attack detected");
		}
*/
	}

	/**
	 * Add simple criteria to the query.
	 * 
	 * @param module
	 * @param document
	 * @param customer
	 * @param query          Add the filter criteria to this query.
	 * @param filterOperator The default operator to use for all filter critiera.
	 * @param criteria       A Map of name value pairs
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
			if (value instanceof String string) {
				value = Util.processStringValue(string);
			}

			binding = BindUtil.unsanitiseBinding(binding);
			String parameterName = null; // unaltered binding value for adding the parameter
			@SuppressWarnings("null") // BindUtil.unsantiseBinding will never return null for non-null argument
			boolean parameter = (binding.charAt(0) == ':');
			if (parameter) {
				binding = binding.substring(1); // lose the colon
				parameterName = binding;
			}

			// Determine the type and converter of the filtered attribute
			Converter<?> converter = null;
			Class<?> type = String.class;

			// Name must be a valid binding if we are adding a filter criteria
			// Not necessarily a valid binding if processing a query parameter
			TargetMetaData target = null;
    		// set to true if equivalence (equals/in) should be used instead of substring/like
			boolean noLikey = false;
			try {
				target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
			}
			catch (MetaDataException e) {
				if (! parameter) {
					throw e;
				}
			}
			if (target != null) {
				Document targetDocument = target.getDocument();
				Attribute attribute = target.getAttribute();
				if (attribute != null) {
					type = attribute.getImplementingType();
					if (attribute instanceof Enumeration enumeration) {
						converter = enumeration.getConverter();
						noLikey = true;
					}

					DomainType domainType = attribute.getDomainType();
					if (domainType == DomainType.constant) {
						noLikey = true;
					}
					else if (domainType == DomainType.variant) {
						if (! parameter) {
							if (value != null) {
								Object[] codes = ListModel.getTop100VariantDomainValueCodesFromDescriptionFilter(targetDocument, attribute, value.toString());
								filter.addIn(binding, codes);
								continue;
							}
						}
					}
					if (attribute instanceof ConvertibleField field) {
						converter = field.getConverterForCustomer(customer);
					}
					else if (attribute instanceof Association association) {
						if (parameter) {
							if (value instanceof String string) {
								Module m = customer.getModule(targetDocument.getOwningModuleName());
								Document d = m.getDocument(customer, association.getDocumentName());
								value = CORE.getPersistence().retrieve(d, string);
							}
							else if (value != null) {
								throw new DomainException(value + " is not supported as an association parameter");
							}
						}
						else {
							noLikey = true;
							type = String.class;
							binding = binding + '.' + Bean.DOCUMENT_ID;
						}
					}
				}
			}

			// What filter operator should I use for this parameter?
			SmartClientFilterOperator fo = filterOperator;

			// Used by this Servlet and SmartClientTagServlet for Enum simple criteria (repeated request params)
			if (value instanceof Object[] values) {
				for (int i = 0, l = values.length; i < l; i++) {
					Object v = values[i];
					if (v != null) {
						v = fromString(binding, "value", v.toString(), customer, converter, type);
						values[i] = v;
					}
				}
				fo = SmartClientFilterOperator.inSet;
			}
			// Used by ReportServlet and ChartServlet for Enum simple criteria (JSON array)
			else if (value instanceof List<?> list) {
				@SuppressWarnings("unchecked")
				List<Object> values = list;
				for (int i = 0, l = values.size(); i < l; i++) {
					Object v = values.get(i);
					if (v != null) {
						v = fromString(binding, "value", v.toString(), customer, converter, type);
						values.set(i, v);
					}
				}
				fo = SmartClientFilterOperator.inSet;
			}
			else if (value != null) {
				// Only convert filter parameters and parameters that aren't beans already
				if (! (parameter && (value instanceof Bean))) {
					value = fromString(binding, "value", value.toString(), customer, converter, type);
					if (noLikey || (value instanceof Date) || (value instanceof Number) || (value instanceof Boolean)) {
						fo = SmartClientFilterOperator.equals;
					}
				}
			}

			if (parameter) {
				model.putParameter(parameterName, value);
			}
			else {
				addCriterionToFilter(binding, fo, value, null, null, tagId, filter);
			}
		}
	}

	/**
	 * Add advanced filter criteria to a query.
	 * 
	 * @param module
	 * @param document
	 * @param customer
	 * @param user
	 * @param query                  The query to add the filter criteria to.
	 * @param compoundFilterOperator The compound filter operator to use between criteria
	 * @param criteria               List of advanced critiera.
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
		// We have to unconditionally add a new filter here as the metadata query might have a filter stanza
		// in it which we can't detect and we need to ensure any filtering done here is 
		// AND'd with any existing criteria (hard-coded in a filter stanza or not)
		Filter filter = model.getFilter();
		Filter newFilter = model.newFilter();
		addAdvancedFilterCriteriaToQueryInternal(module, document, user, compoundFilterOperator, criteria, tagId, model, newFilter);
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
		if (criteria == null) {
			return;
		}

		boolean firstCriteriaIteration = true; // the first filter criteria encountered - not a bound parameter
		for (Map<String, Object> criterion : criteria) {
			if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("criterion = {}", JSON.marshall(criterion));
			String binding = (String) criterion.get("fieldName");
			binding = BindUtil.unsanitiseBinding(binding);
			SmartClientFilterOperator filterOperator = SmartClientFilterOperator.valueOf((String) criterion.get("operator"));

			if (binding == null) { // advanced criteria
				Filter subFilter = model.newFilter();
				CompoundFilterOperator subCompoundFilterOperator = CompoundFilterOperator.valueOf(filterOperator.toString());
				@SuppressWarnings("unchecked")
				List<Map<String, Object>> subCritiera = (List<Map<String, Object>>) criterion.get("criteria");
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
				Object value = criterion.get("value");
				String valueString = null;
				if (value != null) {
					valueString = Util.processStringValue(value.toString());
				}

				String parameterName = null;
				boolean parameter = (binding.charAt(0) == ':');
				if (parameter) {
					binding = binding.substring(1);
					parameterName = binding;
				}

				// Determine the type and converter of the filtered attribute
				Customer customer = user.getCustomer();
				Converter<?> converter = null;
				Class<?> type = String.class;

				TargetMetaData target = null;
				try {
					target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
				}
				catch (MetaDataException e) {
					if (! parameter) {
						throw e;
					}
				}
				if (target != null) {
					Document targetDocument = target.getDocument();
					Attribute attribute = target.getAttribute();
					if (attribute != null) {
						type = attribute.getImplementingType();
						if (attribute instanceof Enumeration enumeration) {
							converter = (enumeration).getConverter();
							filterOperator = transformWildcardFilterOperator(filterOperator);
						}

						DomainType domainType = attribute.getDomainType();
						if (domainType != null) {
							filterOperator = transformWildcardFilterOperator(filterOperator);
							// Translate variant domain filters to a set of codes to search for
							if (! parameter) {
								if ((valueString != null) && (domainType == DomainType.variant)) {
									value = ListModel.getTop100VariantDomainValueCodesFromDescriptionFilter(targetDocument, attribute, valueString);
									filter.addIn(binding, (Object[]) value);
									continue;
								}
							}
						}
						if (attribute instanceof ConvertibleField field) {
							converter = field.getConverterForCustomer(customer);
						}
						else if (attribute instanceof Association association) {
							if (parameter) {
								if (valueString != null) {
									Module m = customer.getModule(targetDocument.getOwningModuleName());
									Document d = m.getDocument(customer, association.getDocumentName());
									value = CORE.getPersistence().retrieve(d, valueString);
								}
							}
							else {
								type = String.class;
								binding = new StringBuilder(binding.length() + 6).append(binding).append('.').append(Bean.DOCUMENT_ID).toString();
								filterOperator = transformWildcardFilterOperator(filterOperator);
							}
						}
					}
					else if (ChildBean.PARENT_NAME.equals(binding) || binding.endsWith(ChildBean.CHILD_PARENT_NAME_SUFFIX)) {
						type = String.class;
						binding = new StringBuilder(binding.length() + 6).append(binding).append('.').append(Bean.DOCUMENT_ID).toString();
					}
					else if (HierarchicalBean.PARENT_ID.equals(binding) || binding.endsWith(HIERARCHICAL_PARENT_ID_SUFFIX)) {
						type = String.class;
					}
				}

				if (value instanceof List<?> list) {
					@SuppressWarnings("unchecked")
					List<Object> values = list;
					for (int i = 0, l = values.size(); i < l; i++) {
						Object v = values.get(i);
						if (v != null) {
							v = fromString(binding, "value", v.toString(), customer, converter, type);
							values.set(i, v);
						}
					}
					// If we got an array and there is no operator (multi-select in filter header),
					// or the operator is "exact" or "equals" (still multi-select in filter header but with other criteria)
					// then set it to inSet
					if ((filterOperator == null) ||
							(filterOperator == SmartClientFilterOperator.equals) ||
							(filterOperator == SmartClientFilterOperator.exact)) {
						filterOperator = SmartClientFilterOperator.inSet;
					}
					// If the operator is notEquals, then set it to notInSet
					else if (filterOperator == SmartClientFilterOperator.notEqual) {
						filterOperator = SmartClientFilterOperator.notInSet;
					}
				}
				else {
					// Only convert filter parameters and parameters that aren't beans already
					if (! (parameter && (value instanceof Bean))) {
						value = fromString(binding, "value", valueString, customer, converter, type);
					}
				}

				if (parameter) {
					model.putParameter(parameterName, value);
					continue;
				}

				Object start = criterion.get("start");
				String startString = null;
				if (start != null) {
					startString = start.toString();
					if ("".equals(startString)) {
						startString = null;
					}
				}

				Object end = criterion.get("end");
				String endString = null;
				if (end != null) {
					endString = end.toString();
					if ("".equals(endString)) {
						endString = null;
					}
				}

				start = fromString(binding, "start", startString, customer, converter, type);
				end = fromString(binding, "end", endString, customer, converter, type);

				// These types do not support String wildcards, so we transform the positive ones to equals and the negative ones to notEqual
				if ((value instanceof Date) ||
						(value instanceof Number) ||
						(value instanceof Boolean) ||
						(start instanceof Date) ||
						(start instanceof Number) ||
						(start instanceof Boolean) ||
						(end instanceof Date) ||
						(end instanceof Number) ||
						(end instanceof Boolean)) {
					filterOperator = transformWildcardFilterOperator(filterOperator);
				}

				switch (compoundFilterOperator) {
					case and:
						addCriterionToFilter(binding, filterOperator, value, start, end, tagId, filter);
						break;
					case or:
						if (firstCriteriaIteration) {
							addCriterionToFilter(binding, filterOperator, value, start, end, tagId, filter);
						}
						else {
							Filter orFilter = model.newFilter();
							addCriterionToFilter(binding, filterOperator, value, start, end, tagId, orFilter);
							if (! orFilter.isEmpty()) {
								filter.addOr(orFilter);
							}
						}
						break;
					case not:
						switch (filterOperator) {
							case substring, contains:
								addCriterionToFilter(binding, SmartClientFilterOperator.notContains, value, start, end, tagId, filter);
								break;
							case iContains:
								addCriterionToFilter(binding, SmartClientFilterOperator.iNotContains, value, start, end, tagId, filter);
								break;
							case notContains:
								addCriterionToFilter(binding, SmartClientFilterOperator.contains, value, start, end, tagId, filter);
								break;
							case iNotContains:
								addCriterionToFilter(binding, SmartClientFilterOperator.iContains, value, start, end, tagId, filter);
								break;
							case startsWith:
								addCriterionToFilter(binding, SmartClientFilterOperator.notStartsWith, value, start, end, tagId, filter);
								break;
							case iStartsWith:
								addCriterionToFilter(binding, SmartClientFilterOperator.iNotStartsWith, value, start, end, tagId, filter);
								break;
							case notStartsWith:
								addCriterionToFilter(binding, SmartClientFilterOperator.startsWith, value, start, end, tagId, filter);
								break;
							case iNotStartsWith:
								addCriterionToFilter(binding, SmartClientFilterOperator.iStartsWith, value, start, end, tagId, filter);
								break;
							case endsWith:
								addCriterionToFilter(binding, SmartClientFilterOperator.notEndsWith, value, start, end, tagId, filter);
								break;
							case iEndsWith:
								addCriterionToFilter(binding, SmartClientFilterOperator.iNotEndsWith, value, start, end, tagId, filter);
								break;
							case notEndsWith:
								addCriterionToFilter(binding, SmartClientFilterOperator.endsWith, value, start, end, tagId, filter);
								break;
							case iNotEndsWith:
								addCriterionToFilter(binding, SmartClientFilterOperator.iEndsWith, value, start, end, tagId, filter);
								break;
							case exact, equals:
								addCriterionToFilter(binding, SmartClientFilterOperator.notEqual, value, start, end, tagId, filter);
								break;
							case iEquals:
								addCriterionToFilter(binding, SmartClientFilterOperator.iNotEqual, value, start, end, tagId, filter);
								break;
							case notEqual:
								addCriterionToFilter(binding, SmartClientFilterOperator.equals, value, start, end, tagId, filter);
								break;
							case iNotEqual:
								addCriterionToFilter(binding, SmartClientFilterOperator.iEquals, value, start, end, tagId, filter);
								break;
							case greaterThan:
								addCriterionToFilter(binding, SmartClientFilterOperator.lessOrEqual, value, start, end, tagId, filter);
								break;
							case greaterOrEqual:
								addCriterionToFilter(binding, SmartClientFilterOperator.lessThan, value, start, end, tagId, filter);
								break;
							case lessThan:
								addCriterionToFilter(binding, SmartClientFilterOperator.greaterOrEqual, value, start, end, tagId, filter);
								break;
							case lessOrEqual:
								addCriterionToFilter(binding, SmartClientFilterOperator.greaterThan, value, start, end, tagId, filter);
								break;
							case betweenInclusive, iBetweenInclusive:
								if (start != null) {
									addCriterionToFilter(binding, SmartClientFilterOperator.lessOrEqual, start, null, null, tagId, filter);
								}
								if (end != null) {
									addCriterionToFilter(binding, SmartClientFilterOperator.greaterThan, end, null, null, tagId, filter);
								}
								break;
							case iBetween:
								if (start != null) {
									addCriterionToFilter(binding, SmartClientFilterOperator.lessOrEqual, start, null, null, tagId, filter);
								}
								if (end != null) {
									addCriterionToFilter(binding, SmartClientFilterOperator.greaterOrEqual, end, null, null, tagId, filter);
								}
								break;
							case isNull:
								addCriterionToFilter(binding, SmartClientFilterOperator.notNull, value, start, end, tagId, filter);
								break;
							case notNull:
								addCriterionToFilter(binding, SmartClientFilterOperator.isNull, value, start, end, tagId, filter);
								break;
							case isBlank:
								addCriterionToFilter(binding, SmartClientFilterOperator.notBlank, value, start, end, tagId, filter);
								break;
							case notBlank:
								addCriterionToFilter(binding, SmartClientFilterOperator.notBlank, value, start, end, tagId, filter);
								break;
							case equalsField:
								addCriterionToFilter(binding, SmartClientFilterOperator.notEqualField, value, start, end, tagId, filter);
								break;
							case iEqualsField:
								addCriterionToFilter(binding, SmartClientFilterOperator.iNotEqualField, value, start, end, tagId, filter);
								break;
							case notEqualField:
								addCriterionToFilter(binding, SmartClientFilterOperator.equalsField, value, start, end, tagId, filter);
								break;
							case iNotEqualField:
								addCriterionToFilter(binding, SmartClientFilterOperator.iEqualsField, value, start, end, tagId, filter);
								break;
							case containsField:
								addCriterionToFilter(binding, SmartClientFilterOperator.notContainsField, value, start, end, tagId, filter);
								break;
							case iContainsField:
								addCriterionToFilter(binding, SmartClientFilterOperator.iNotContainsField, value, start, end, tagId, filter);
								break;
							case notContainsField:
								addCriterionToFilter(binding, SmartClientFilterOperator.containsField, value, start, end, tagId, filter);
								break;
							case iNotContainsField:
								addCriterionToFilter(binding, SmartClientFilterOperator.iContainsField, value, start, end, tagId, filter);
								break;
							case startsWithField:
								addCriterionToFilter(binding, SmartClientFilterOperator.notStartsWithField, value, start, end, tagId, filter);
								break;
							case iStartsWithField:
								addCriterionToFilter(binding, SmartClientFilterOperator.iNotStartsWithField, value, start, end, tagId, filter);
								break;
							case notStartsWithField:
								addCriterionToFilter(binding, SmartClientFilterOperator.startsWithField, value, start, end, tagId, filter);
								break;
							case iNotStartsWithField:
								addCriterionToFilter(binding, SmartClientFilterOperator.iStartsWithField, value, start, end, tagId, filter);
								break;
							case endsWithField:
								addCriterionToFilter(binding, SmartClientFilterOperator.notEndsWithField, value, start, end, tagId, filter);
								break;
							case iEndsWithField:
								addCriterionToFilter(binding, SmartClientFilterOperator.iNotEndsWithField, value, start, end, tagId, filter);
								break;
							case notEndsWithField:
								addCriterionToFilter(binding, SmartClientFilterOperator.endsWithField, value, start, end, tagId, filter);
								break;
							case iNotEndsWithField:
								addCriterionToFilter(binding, SmartClientFilterOperator.iEndsWithField, value, start, end, tagId, filter);
								break;
							case greaterThanField:
								addCriterionToFilter(binding, SmartClientFilterOperator.lessOrEqualField, value, start, end, tagId, filter);
								break;
							case greaterOrEqualField:
								addCriterionToFilter(binding, SmartClientFilterOperator.lessThanField, value, start, end, tagId, filter);
								break;
							case lessThanField:
								addCriterionToFilter(binding, SmartClientFilterOperator.greaterOrEqualField, value, start, end, tagId, filter);
								break;
							case lessOrEqualField:
								addCriterionToFilter(binding, SmartClientFilterOperator.greaterThanField, value, start, end, tagId, filter);
								break;
							case inSet:
								addCriterionToFilter(binding, SmartClientFilterOperator.notInSet, value, start, end, tagId, filter);
								break;
							case notInSet:
								addCriterionToFilter(binding, SmartClientFilterOperator.inSet, value, start, end, tagId, filter);
								break;
							case regexp, iregexp:
								// nothing to do
								break;
							case containsPattern:
							case iContainsPattern:
							case matchesPattern:
							case iMatchesPattern:
							case startsWithPattern:
							case iStartsWithPattern:
							case endsWithPattern:
							case iEndsWithPattern:
								// nothing to do
								break;
							case geoContains:
							case geoCrosses:
							case geoDisjoint:
							case geoEquals:
							case geoIntersects:
							case geoOverlaps:
							case geoTouches:
							case geoWithin:
								// TODO fix this later
								break;
							case and, or, not:
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
					result = BindUtil.fromSerialised(type, valueString + "T00:00:00.000");
				}
				else {
					result = BindUtil.fromSerialised(converter, type, valueString);
				}
			}
			catch (Exception e) {
				try {
					result = BindUtil.fromString(customer, converter, type, valueString);
				}
				catch (Exception e1) {
					LOGGER.warn("Could not convert {} as type {} with converter {}. See the following stack traces below", valueString, type, converter, e1);
					e.printStackTrace();
					e1.printStackTrace();
					if (valueBinding == null) {
						throw new ValidationException(new Message("Please enter a properly formatted " + valueDescription));
					}
					throw new ValidationException(new Message(valueBinding.replace('.', '_'), "Please enter a properly formatted " + valueDescription));
				}
			}
		}

		if (result instanceof String string) {
			result = string.replace('\'', '%'); // remove ' delimiters;
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

	private static void addCriterionToFilter(String binding,
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
			if (value instanceof org.skyve.domain.types.Enumeration enumeration) {
				revisedValue = enumeration.toCode();
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
					case substring, iContains, contains:
						if (revisedValue != null) {
							filter.addContains(binding, revisedValue.toString());
						}
						break;
					case iNotContains, notContains:
						if (revisedValue != null) {
							filter.addNotContains(binding, revisedValue.toString());
						}
						break;
					case startsWith, iStartsWith:
						if (revisedValue != null) {
							filter.addStartsWith(binding, revisedValue.toString());
						}
						break;
					case iNotStartsWith, notStartsWith:
						if (revisedValue != null) {
							filter.addNotStartsWith(binding, revisedValue.toString());
						}
						break;
					case iEndsWith, endsWith:
						if (revisedValue != null) {
							filter.addEndsWith(binding, revisedValue.toString());
						}
						break;
					case iNotEndsWith, notEndsWith:
						if (revisedValue != null) {
							filter.addNotEndsWith(binding, revisedValue.toString());
						}
						break;
					case equals, exact:
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
					case iBetweenInclusive, betweenInclusive, iBetween:
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
					case isNull, isBlank:
						filter.addNull(binding);
						break;
					case notNull, notBlank:
						filter.addNotNull(binding);
						break;
					case equalsField:
					case iEqualsField:
					case notEqualField:
					case iNotEqualField:
					case containsField:
					case iContainsField:
					case notContainsField:
					case iNotContainsField:
					case startsWithField:
					case iStartsWithField:
					case notStartsWithField:
					case iNotStartsWithField:
					case endsWithField:
					case iEndsWithField:
					case notEndsWithField:
					case iNotEndsWithField:
					case greaterOrEqualField:
					case greaterThanField:
					case lessOrEqualField:
					case lessThanField:
						// TODO
						break;
					case matchesPattern:
					case iMatchesPattern:
					case containsPattern:
					case iContainsPattern:
					case startsWithPattern:
					case iStartsWithPattern:
					case endsWithPattern:
					case iEndsWithPattern:
						// TODO
						break;
					case regexp: // Regular expression match
					case iregexp: // Regular expression match (case insensitive)
						break;
					case inSet: // value is in a set of values. Specify criterion.value as an Array
						if (value instanceof Object[] values) {
							filter.addIn(binding, values);
						}
						else if (value instanceof List<?> values) {
							filter.addIn(binding, values.toArray());
						}
						break;
					case notInSet: // value is not in a set of values. Specify criterion.value as an Array
						if (value instanceof Object[] values) {
							filter.addNotIn(binding, values);
						}
						else if (value instanceof List<?> values) {
							filter.addNotIn(binding, values.toArray());
						}
						break;
					case geoWithin:
						if (value instanceof Geometry geometry) {
							filter.addWithin(binding, geometry);
						}
						break;
					case geoContains:
						if (value instanceof Geometry geometry) {
							filter.addContains(binding, geometry);
						}
						break;
					case geoCrosses:
						if (value instanceof Geometry geometry) {
							filter.addCrosses(binding, geometry);
						}
						break;
					case geoDisjoint:
						if (value instanceof Geometry geometry) {
							filter.addDisjoint(binding, geometry);
						}
						break;
					case geoEquals:
						if (value instanceof Geometry geometry) {
							filter.addEquals(binding, geometry);
						}
						break;
					case geoIntersects:
						if (value instanceof Geometry geometry) {
							filter.addIntersects(binding, geometry);
						}
						break;
					case geoOverlaps:
						if (value instanceof Geometry geometry) {
							filter.addOverlaps(binding, geometry);
						}
						break;
					case geoTouches:
						if (value instanceof Geometry geometry) {
							filter.addTouches(binding, geometry);
						}
						break;
					case and, not, or:
						break;
					default:
				}
			}
		}
	}

/*
    private void add(Module module, 
						ListModel<Bean> model,
						boolean rowIsTagged,
						SortedMap<String, Object> properties, 
						AbstractPersistence persistence,
						PrintWriter pw)
	throws Exception {
		StringBuilder message = new StringBuilder(256);

		// create a new document
		// add the parameters given
		Bean bean = model.add(bizId, properties);

		// return the updated row
		pw.append(returnUpdatedMessage(customer, model, bean, rowIsTagged));
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

			if ((! (column instanceof MetaDataQueryProjectedColumn projected)) || (! projected.isEditable())) {
				properties.remove(columnBinding);
			}
			else {
				// replace association bizIds with the real object
				TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, columnBinding);
				Attribute targetAttribute = target.getAttribute();
				if (targetAttribute instanceof Association association) {
					String associationId = (String) properties.get(columnBinding);
					if (associationId == null) {
						properties.put(columnBinding, null);
					}
					else {
						properties.put(columnBinding,
										persistence.retrieve(module.getName(),
																association.getDocumentName(),
																associationId));
					}
				}
			}
		}

		String bizId = (String) properties.remove(Bean.DOCUMENT_ID);
		Bean bean = model.update(bizId, properties);

		// return the updated row
		Util.chunkCharsToWriter(returnUpdatedMessage(user, customer, module, document, model, bean, rowIsTagged), pw);
	}

	private static void tag(User user,
								Customer customer,
								Module module,
								ListModel<Bean> model,
								String tagId,
								Map<String, Object> parameters,
								PrintWriter pw)
	throws Exception {
		String bizId = (String) parameters.get(Bean.DOCUMENT_ID);
		EXT.getTagManager().tag(tagId, module.getName(), model.getDrivingDocument().getName(), bizId);

		// return the updated row
		Util.chunkCharsToWriter(returnTagUpdateMessage(user, customer, parameters, module, model, true), pw);
	}

	private static void untag(User user,
								Customer customer,
								Module module,
								ListModel<Bean> model,
								String tagId,
								Map<String, Object> parameters,
								PrintWriter pw)
	throws Exception {
		String bizId = (String) parameters.get(Bean.DOCUMENT_ID);
		EXT.getTagManager().untag(tagId, module.getName(), model.getDrivingDocument().getName(), bizId);

		// return the updated row
		Util.chunkCharsToWriter(returnTagUpdateMessage(user, customer, parameters, module, model, false), pw);
	}

	private static void flag(HttpServletRequest request,
								PrintWriter pw,
								AbstractPersistence persistence,
								User user,
								Customer customer,
								Module module,
								Document drivingDocument,
								ListModel<Bean> model,
								SortedMap<String, Object> parameters,
								String bizFlagComment)
	throws Exception {
		String bizId = (String) parameters.get(Bean.DOCUMENT_ID);
		Bean bean = persistence.retrieve(drivingDocument, bizId);
		if (bean == null) {
			throw new NoResultsException();
		}
		BindUtil.set(bean, PersistentBean.FLAG_COMMENT_NAME, bizFlagComment);
		upsertFlag(drivingDocument, bean, bizFlagComment);

		Util.chunkCharsToWriter(returnUpdatedMessage(user, customer, module, drivingDocument, model, bean, isRowTagged(request)), pw);
	}

	private static StringBuilder returnUpdatedMessage(User user,
														Customer customer,
														Module module,
														Document document,
														ListModel<Bean> model, 
														Bean bean,
														boolean rowIstagged)
	throws Exception {
		StringBuilder message = new StringBuilder(256);
		message.append("{\"response\":{\"status\":0,\"data\":");

		// Nullify flag comment if not given permissions
		if (! user.canFlag()) {
			BindUtil.set(bean, PersistentBean.FLAG_COMMENT_NAME, null);
		}

		Set<String> projections = processRows(Collections.singletonList(bean), model, user, customer, module, document);
		String json = JSON.marshall(customer, bean, projections);

		// reinstate whether the record is tagged or not.
		if (rowIstagged) {
			json = json.replace(PersistentBean.TAGGED_NAME + "\":null", PersistentBean.TAGGED_NAME + "\":true");
		}

		message.append(json);
		message.append("}}");

		return message;
	}

	private static StringBuilder returnTagUpdateMessage(User user,
															Customer customer,
															Map<String, Object> parameters,
															Module module,
															ListModel<Bean> model,
															boolean tagging)
	throws Exception {
		StringBuilder message = new StringBuilder(256);
		message.append("{\"response\":{\"status\":0,\"data\":[");

		boolean canFlag = user.canFlag();

		Set<String> projections = model.getProjections();
		Map<String, Object> properties = new TreeMap<>();
		for (String projection : projections) {
			if (PersistentBean.TAGGED_NAME.equals(projection)) {
				properties.put(projection, Boolean.valueOf(tagging));
			}
			else if ((! canFlag) && PersistentBean.FLAG_COMMENT_NAME.equals(projection)) {
				// Nullify flag comments
				properties.put(projection, null);
			}
			else {
				properties.put(projection, parameters.get(projection));
			}
		}

		message.append(JSON.marshall(customer,
										new DynamicBean(module.getName(),
															model.getDrivingDocument().getName(),
															properties),
										projections));
		message.append("]}}");

		return message;
	}

	private static void remove(ListModel<Bean> model,
								Map<String, Object> parameters, 
								PrintWriter pw)
	throws Exception {
		model.remove((String) parameters.get(Bean.DOCUMENT_ID));
		pw.append("{\"response\":{\"status\":0}}");
	}

	private static void upsertFlag(Document drivingDocument, Bean bean, String flag) {
		StringBuilder sql = new StringBuilder(64);
		@SuppressWarnings("null")
		String persistentIdentifier = drivingDocument.getPersistent().getPersistentIdentifier();
		sql.append("update ").append(persistentIdentifier).append(" set ").append(PersistentBean.FLAG_COMMENT_NAME)
				.append("= :").append(PersistentBean.FLAG_COMMENT_NAME).append(" where ").append(Bean.DOCUMENT_ID)
				.append("= :").append(Bean.DOCUMENT_ID);
		CORE.getPersistence().newSQL(sql.toString())
				.putParameter(PersistentBean.FLAG_COMMENT_NAME, flag, false)
				.putParameter(Bean.DOCUMENT_ID, bean.getBizId(), false).execute();
	}

	private static boolean isRowTagged(HttpServletRequest request) {
		boolean rowIsTagged = false;
		String oldValuesJSON = request.getParameter(OLD_VALUES);
		if (oldValuesJSON != null) {
			rowIsTagged = oldValuesJSON.contains(PersistentBean.TAGGED_NAME + "\":true");
		}

		return rowIsTagged;
	}
}