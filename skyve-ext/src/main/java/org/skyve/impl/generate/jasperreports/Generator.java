package org.skyve.impl.generate.jasperreports;

import java.util.HashMap;

import org.skyve.CORE;
import org.skyve.impl.generate.jasperreports.DesignSpecification.DefinitionSource;
import org.skyve.impl.generate.jasperreports.DesignSpecification.Mode;
import org.skyve.impl.generate.jasperreports.DesignSpecification.ReportType;
import org.skyve.impl.generate.jasperreports.ReportBand.BandType;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

public class Generator {

	protected ReportViewVisitor visitor = null;
	private DesignSpecification design = null;
	private Module module = null;
	private Document document = null;

	public ReportViewVisitor getVisitor() {
		return visitor;
	}

	public void setVisitor(ReportViewVisitor visitor) {
		this.visitor = visitor;
	}

	/**
	 * Convenience constructor for design and visitor
	 * 
	 * @param design
	 * @param visitor
	 */
	public Generator(DesignSpecification design, ReportViewVisitor visitor) {
		this(design);
		this.visitor = visitor;
	}

	/**
	 * Constructor given a design
	 * 
	 * @param design
	 */
	public Generator(DesignSpecification design) {

		this.design = design;

		Persistence pers = CORE.getPersistence();
		Customer customer = pers.getUser().getCustomer();
		module = customer.getModule(design.getModuleName());
		document = module.getDocument(customer, design.getDocumentName());

		if (DefinitionSource.view.equals(design.getDefinitionSource())) {

			View view = document.getView(design.getUxui(), customer, ViewType.edit.toString());
			visitor = new ReportViewVisitor((CustomerImpl) customer, (ModuleImpl) module, (DocumentImpl) document, (ViewImpl) view);
		}
	}

	public void setDesign(DesignSpecification design) {
		this.design = design;
	}

	public DesignSpecification getDesign() {
		return design;
	}

	/**
	 * Create a default report according to the design
	 * 
	 * @param bean
	 * @param keepFields
	 *            - if keep fields is false, then do a complete generate from scratch, otherwise, keep manual field modifications
	 * @return
	 * @throws Exception
	 */
	public void generateDefaultDesign() throws Exception {

		design.setJoins(new HashMap<>());
		design.setJoinAlias(new HashMap<>());
		design.setAlias((int) ('a'));

		if (design.getModuleName() != null && design.getDocumentName() != null) {
			design.getBands().clear();
			design.getFields().clear();
			design.getParameters().clear();
			design.getVariables().clear();
			design.getSubReports().clear();

			generateDefaultParameters();
			generateDefaultFields();
			generateDefaultVariables();
			generateDefaultSubreports();
			generateDefaultBands();
		}
	}

	protected void generateDefaultParameters() throws Exception {

		ReportParameter p1 = new ReportParameter();
		p1.setName("SUBREPORT_DIR");
		p1.setTypeClass("java.lang.String");

		String pathToReport = Renderer.pathToReport(design.getModuleName(), design.getDocumentName(), true);
		p1.setDefaultValueExpression(pathToReport);
		design.getParameters().add(p1);

		ReportParameter p2 = new ReportParameter();
		p2.setName("RESOURCE_DIR");
		p2.setTypeClass("java.lang.String");
		p2.setDefaultValueExpression("");
		design.getParameters().add(p2);

		ReportParameter p3 = new ReportParameter();
		p3.setName("ID");
		p3.setTypeClass("java.lang.String");
		design.getParameters().add(p3);
	}

	protected void generateDefaultFields() throws Exception {

		DesignSpecification result = design;

		Customer customer = CORE.getPersistence().getUser().getCustomer();
		Module module = customer.getModule(document.getOwningModuleName());

		if (ReportType.report.equals(design.getReportType())) {
			if (Mode.bean.equals(design.getMode())) {
				ReportField fThis = new ReportField();
				fThis.setParent(design);
				fThis.setName("THIS");
				fThis.setTypeClass("modules." + module.getName() + ".domain." + document.getName());
				fThis.setImplicit(Boolean.TRUE);
				result.getFields().add(fThis);

				ReportField fUser = new ReportField();
				fUser.setName("USER");
				fUser.setParent(design);
				fUser.setTypeClass("org.skyve.metadata.user.User");
				fUser.setImplicit(Boolean.TRUE);
				result.getFields().add(fUser);
			}

			ReportField fbK = new ReportField();
			fbK.setName("bizKey");
			fbK.setTypeClass("java.lang.String");
			fbK.setImplicit(Boolean.TRUE);

			Document extDocument = null;
			if (document.getExtends() != null && document.getExtends().getDocumentName() != null && ExtensionStrategy.joined.equals(document.getPersistent().getStrategy())) {
				extDocument = module.getDocument(customer, document.getExtends().getDocumentName());

				design.setAlias(design.getAlias() + 1);
				StringBuilder sJoin = new StringBuilder();
				sJoin.append(" join ").append(Renderer.getPersistentFromDocument(extDocument)).append(" ").append((char) design.getAlias());
				sJoin.append(" on ").append("a.bizId = ").append((char) design.getAlias()).append(".bizId");

				design.addJoin(extDocument.getName(), Character.toString((char) design.getAlias()), sJoin.toString());
				fbK.setJoinSql(sJoin.toString());
				fbK.setNameSql(design.getJoinAlias().get(sJoin.toString()) + ".bizKey ");
			} else {
				fbK.setNameSql("a.bizKey");
			}

			result.getFields().add(fbK);
		}

		// add other fields
		if (DefinitionSource.document.equals(design.getDefinitionSource())) {

			for (Attribute a : document.getAttributes()) {
				if (!a.isDeprecated() && (a.isPersistent() || Mode.bean.equals(design.getMode()))) {

					ReportField fld = fieldFromAttribute(design, customer, document, a, new StringBuilder(), new StringBuilder());
					result.getFields().add(fld);
				}
			}
		} else if (DefinitionSource.view.equals(design.getDefinitionSource())) {

			visitor.setDesign(design);
			visitor.visit();
		}
	}

	/**
	 * construct a report field from a document attribute
	 * 
	 * @param bean
	 * @param customer
	 * @param document
	 * @param a
	 * @return
	 * @throws Exception
	 */
	protected static ReportField fieldFromAttribute(DesignSpecification bean, Customer customer, Document document, Attribute a, StringBuilder sJoin, StringBuilder fieldPrefix)
			throws Exception {

		ReportField f = new ReportField();
		f.setParent(bean);
		switch (a.getAttributeType()) {
		case collection:
			f.setName(a.getName());

			f.setTypeClass("java.util.List");

			Collection c = (Collection) a;
			f.setOwningModuleName(document.getOwningModuleName());
			f.setDocumentName(c.getDocumentName());
			f.setCollection(Boolean.TRUE);
			break;
		case association:

			if (Mode.bean.equals(bean.getMode())) {
				f.setName(a.getName() + ".bizKey");
			} else {
				f.setName(a.getName() + "_bizKey");
				f.setNameSql(a.getName() + ".bizKey as " + f.getName());
			}

			sJoin = addJoinForAssociation(new StringBuilder(), customer, bean, document, a, a.getName());
			f.setJoinSql(sJoin.toString());

			f.setTypeClass("java.lang.String");
			break;
		default:
			fieldPrefix.append(a.getName());
			f.setName(fieldPrefix.toString());
			if (sJoin.length() == 0) {
				f.setNameSql("a." + f.getName());
			} else {
				f.setJoinSql(sJoin.toString());
				f.setNameSql(bean.getJoinAlias().get(sJoin.toString()) + "." + a.getName() + " as " + f.getName());
			}

			if (Mode.bean.equals(bean.getMode())) {

				f.setTypeClass("java.lang.String"); // Binder will convert to String
			} else {
				f.setTypeClass(Renderer.getSqlEquivalentClass(a.getAttributeType()));
			}

			if (AttributeType.decimal2.equals(a.getAttributeType()) || AttributeType.decimal5.equals(a.getAttributeType())
					|| AttributeType.decimal10.equals(a.getAttributeType())
					|| AttributeType.integer.equals(a.getAttributeType())
					|| AttributeType.longInteger.equals(a.getAttributeType())) {
				f.setIncludeTotal(Boolean.TRUE);
			}

			break;
		}
		f.setSkyveType(a.getAttributeType().name());
		f.setDisplayName(a.getDisplayName());

		return f;
	}

	/**
	 * Constructs a field with joins for a given binding
	 * 
	 * @param design
	 * @param customer
	 * @param document
	 * @param binding
	 * @return
	 * @throws Exception
	 */
	protected static ReportField fieldFromBinding(DesignSpecification design, Customer customer, Document document, String binding) {

		ReportField result = null;
		StringBuilder sJoin = new StringBuilder();
		StringBuilder prefix = new StringBuilder();
		try {

			if (binding.contains(".")) {

				String[] bindings = binding.split("\\.");
				String uniqueJoinIdentifier = "";
				for (int i = 0; i < bindings.length; i++) {
					Attribute a = document.getAttribute(bindings[i]);

					if (Mode.sql.equals(design.getMode()) && !a.isPersistent()) {
						// abandon creating a field - sql reports can't report on non persistent fields
						break;
					} else {

						// build up join string
						if (AttributeType.association.equals(a.getAttributeType())) {
							uniqueJoinIdentifier = uniqueJoinIdentifier + ":" + a.getName();

							sJoin = addJoinForAssociation(sJoin, customer, design, document, a, uniqueJoinIdentifier);
							prefix.append(bindings[i]).append("_");
						} else {
							result = fieldFromAttribute(design, customer, document, a, sJoin, prefix);
						}
					}
				}
			} else {
				Attribute a = document.getAttribute(binding);
				if (Mode.sql.equals(design.getMode())
						&& !a.isPersistent()) {
					// do nothing for sql report with non persistent attribute
				} else {
					result = fieldFromAttribute(design, customer, document, a, new StringBuilder(), new StringBuilder());
				}
			}
		} catch (Exception e) {
			Util.LOGGER.warning("COULD NOT CONSTRUCT FIELD FROM BINDING " + binding + " FOR DOCUMENT " + document.getName());
		}
		if (result != null) {
			result.setBinding(binding);
		}

		return result;
	}

	/**
	 * Construct an sql join for the association
	 * 
	 * @param sJoin
	 * @param customer
	 * @param bean
	 * @param document
	 * @param a
	 * @return
	 */
	protected static StringBuilder addJoinForAssociation(StringBuilder sJoin, Customer customer, DesignSpecification bean, Document document, Attribute a,
			String uniqueJoinIdentifier) {

		if (!bean.getJoins().containsKey(document.getName())) {
			// get the next alias
			bean.setAlias(bean.getAlias() + 1);
		}

		Module fMod = customer.getModule(bean.getModuleName());
		Association assoc = (Association) a;
		Document fDoc = fMod.getDocument(customer, assoc.getDocumentName());
		if (!a.isRequired()) {
			sJoin.append(" left join ");
		} else {
			sJoin.append(" join ");
		}
		sJoin.append(Renderer.getPersistentFromDocument(fDoc));
		sJoin.append(" ").append((char) bean.getAlias());
		sJoin.append(" on ").append("a.bizId = ");
		sJoin.append(" ").append((char) bean.getAlias()).append(".").append(a.getName()).append("_id");

		if (!bean.getJoins().containsKey(document.getName())) {
			bean.addJoin(document.getName() + ":" + uniqueJoinIdentifier, Character.toString((char) bean.getAlias()), sJoin.toString());
		}

		return sJoin;
	}

	/**
	 * Generate a subReport for each collection type attribute
	 * 
	 * @param document
	 * @param bean
	 * @return
	 * @throws Exception
	 */
	protected void generateDefaultSubreports() throws Exception {

		//only generate default reports if subreports is empty (allows for manual creation of subreports instead of default)
		if (design.getSubReports().isEmpty()) {

			for (ReportField a : design.getFields()) {
				if (ReportType.report.equals(design.getReportType())
						&& AttributeType.collection.name().equals(a.getSkyveType())) {

					constructSubreportFromField(a, design.getColumnWidth());

				}
			}
		}
	}

	protected void generateDefaultVariables() throws Exception {

		if (ReportType.subreport.equals(design.getReportType())) {
			for (ReportField a : design.getFields()) {
				if (!AttributeType.collection.name().equals(a.getSkyveType()) && !Boolean.TRUE.equals(a.getImplicit())) {
					if (Boolean.TRUE.equals(a.getIncludeTotal())) {
						ReportVariable v = new ReportVariable();
						v.setParent(design);
						v.setTypeClass(a.getTypeClass());
						v.setName(a.getName());

						design.getVariables().add(v);
					}
				}
			}
		}
	}

	protected void generateDefaultBands() throws Exception {

		ReportBand background = new ReportBand();
		background.setBandType(ReportBand.BandType.background);
		background.setParent(design);
		design.getBands().add(background);

		if (DefinitionSource.document.equals(design.getDefinitionSource())) {

			if (ReportType.report.equals(design.getReportType())) {

				ReportBand title = new ReportBand();
				title.setBandType(ReportBand.BandType.title);
				title.setName("Title");
				title.setParent(design);

				// TODO handle borders as stretchable lines
				title = Renderer.addElement(title, ReportElement.ElementType.textField, "bizKey", Renderer.renderBoundMessage(design, "{bizKey}"), null, null, new Integer(0),
						new Integer(0), design.getColumnWidth(),
						design.getDefaultElementHeight() * 2, null, null, Boolean.TRUE, null, null, null, null);

				if (Boolean.TRUE.equals(design.getSectionBorderTop())
						|| Boolean.TRUE.equals(design.getSectionBorderLeft())
						|| Boolean.TRUE.equals(design.getSectionBorderBottom())
						|| Boolean.TRUE.equals(design.getSectionBorderRight())) {

					// TODO implement borders as lines
					title = Renderer.addElement(title, ReportElement.ElementType.border, null, null, null, null, new Integer(0), new Integer(0), design.getColumnWidth(),
							design.getDefaultElementHeight() * 2, null, null, null, null, null, null, null);
				}

				design.getBands().add(title);
			}
		} else if (DefinitionSource.view.equals(design.getDefinitionSource())) {

			if (!visitor.isVisited()) {
				visitor.visit();
			}

			// construct title band
			if (ReportType.report.equals(design.getReportType())) {
				ReportBand title = new ReportBand();
				title.setBandType(ReportBand.BandType.title);
				title.setName("Title");
				title.setParent(design);

				title = Renderer.addElement(title, ReportElement.ElementType.textField, "title", Renderer.renderBoundMessage(design, visitor.getViewTitle()), null, null,
						new Integer(0), new Integer(0), design.getColumnWidth(),
						design.getDefaultElementHeight() * 2, null, null, Boolean.TRUE, null, null, null, null);

				if (Boolean.TRUE.equals(design.getSectionBorderTop())
						|| Boolean.TRUE.equals(design.getSectionBorderLeft())
						|| Boolean.TRUE.equals(design.getSectionBorderBottom())
						|| Boolean.TRUE.equals(design.getSectionBorderRight())) {

					// TODO implement borders as lines
					title = Renderer.addElement(title, ReportElement.ElementType.border, null, null, null, null, new Integer(0), new Integer(0), design.getColumnWidth(),
							design.getDefaultElementHeight() * 2, null, null, null, null, design.getSectionTitleForeground(), design.getSectionTitleBackground(), null);
				}

				design.getBands().add(title);
			}
		}

		ReportBand pageHeader = new ReportBand();
		pageHeader.setBandType(ReportBand.BandType.pageHeader);
		pageHeader.setParent(design);
		design.getBands().add(pageHeader);

		ReportBand columnHeader = new ReportBand();
		columnHeader.setBandType(ReportBand.BandType.columnHeader);
		columnHeader.setName(BandType.columnHeader.toString());
		columnHeader.setParent(design);

		if (DefinitionSource.document.equals(design.getDefinitionSource())) {

			// grid layout headers
			if (DesignSpecification.ReportType.subreport.equals(design.getReportType())) {

				for (ReportField a : design.getFields()) {

					if (!AttributeType.collection.name().equals(a.getSkyveType()) && !Boolean.TRUE.equals(a.getImplicit())) {

						ReportElement.ElementType type = ReportElement.ElementType.staticText;
						String valueExpression = a.getDisplayName();
						ReportElement.ElementAlignment alignment = ReportElement.ElementAlignment.left;

						if (Boolean.TRUE.equals(design.getRenderLabelAsTextFields()) || Boolean.TRUE.equals(design.getDynamicFlow())) {
							type = ReportElement.ElementType.textField;
							valueExpression = "\"" + a.getDisplayName() + "\"";
						}
						if (Boolean.TRUE.equals(a.getIncludeTotal())) {
							alignment = ReportElement.ElementAlignment.right;
						}
						columnHeader = Renderer.addElement(columnHeader, type, a.getName(), valueExpression, null, null, new Integer(0), null, null, null, null, alignment,
								design.getBoldLabels(), null, null, null, null);
					}
				}
				columnHeader.spreadElements();
			}
			design.getBands().add(columnHeader);

			// add a detail band for every non collection attribute
			switch (design.getReportType()) {
			case subreport:

				ReportBand subReportDetail = new ReportBand();
				subReportDetail.setBandType(ReportBand.BandType.detail);
				subReportDetail.setParent(design);

				// grid layout data
				for (ReportField srf : design.getFields()) {
					if (!Boolean.TRUE.equals(srf.getImplicit())) {
						ReportElement.ElementAlignment alignment = ReportElement.ElementAlignment.left;
						String valueExpression = "$F{" + srf.getName() + "}";

						subReportDetail = Renderer.addElement(subReportDetail, ReportElement.ElementType.textField, srf.getName(), valueExpression, null, null, new Integer(0),
								new Integer(0), design.getColumnWidth(), null, null, alignment, null, null, null, null, null);
					}
				}
				subReportDetail.spreadElements();

				design.getBands().add(subReportDetail);

				break;

			case report:
				for (ReportField a : design.getFields()) {
					if (!AttributeType.collection.name().equals(a.getSkyveType()) && !Boolean.TRUE.equals(a.getImplicit())) {
						ReportBand detail = new ReportBand();
						detail.setBandType(ReportBand.BandType.detail);
						detail.setParent(design);
						detail.setName(a.getName());

						ReportElement.ElementType type = ReportElement.ElementType.staticText;
						String valueExpression = a.getDisplayName();
						ReportElement.ElementAlignment alignment = ReportElement.ElementAlignment.left;
						Integer labelWidth = new Integer(design.getDefaultElementHeight().intValue() * 5);
						Integer top = new Integer(0);
						Integer labelLeft = new Integer(0);

						// add label
						if (Boolean.TRUE.equals(design.getRenderLabelAsTextFields()) || Boolean.TRUE.equals(design.getDynamicFlow())) {
							type = ReportElement.ElementType.textField;
							valueExpression = "\"" + a.getDisplayName() + "\"";
						}
						if (Boolean.TRUE.equals(a.getIncludeTotal())) {
							alignment = ReportElement.ElementAlignment.right;
						}

						detail = Renderer.addElement(detail, type, a.getName(), valueExpression, null, null, top, labelLeft, labelWidth, null, null, alignment,
								design.getBoldLabels(), null, null, null, null);

						// add value
						type = ReportElement.ElementType.textField;
						valueExpression = "$F{" + a.getName() + "}";
						int valueWidth = design.getColumnWidth().intValue() - labelWidth;

						detail = Renderer.addElement(detail, type, a.getName(), valueExpression, null, null, top, labelWidth, valueWidth, null, null, alignment, null,
								null, null, null, null);

						design.getBands().add(detail);
					}
				}

				// add detail bands for subReports
				for (DesignSpecification sub : design.getSubReports()) {

					ReportBand subBand = new ReportBand();
					subBand.setBandType(ReportBand.BandType.detail);
					subBand.setParent(design);
					subBand.setName(sub.getName());

					ReportElement eS = new ReportElement(ReportElement.ElementType.subreport,
							sub.getField().getName(),
							null,
							new Integer(0),
							new Integer(0),
							design.getColumnWidth(),
							null);
					eS.setReportFileName(sub.getName());
					eS.setParent(subBand);

					eS.setCollectionDocumentName(sub.getDocumentName());

					eS.setElementHeight(design.getDefaultElementHeight());
					eS.setDynamicFlow(design.getDynamicFlow());

					eS.setParent(subBand);
					subBand.getElements().add(eS);
					design.getBands().add(subBand);
				}

				break;
			}

			ReportBand columnFooter = new ReportBand();
			columnFooter.setBandType(ReportBand.BandType.columnFooter);
			columnFooter.setParent(design);

			// grid footers - totals expression
			if (ReportType.subreport.equals(design.getReportType())) {
				for (ReportField a : design.getFields()) {
					if (!AttributeType.collection.name().equals(a.getSkyveType()) && !Boolean.TRUE.equals(a.getImplicit())) {

						String valueExpression = "";
						ReportElement.ElementAlignment alignment = null;
						if (Boolean.TRUE.equals(a.getIncludeTotal())) {
							valueExpression = "$V{" + a.getName() + "}";
							alignment = ReportElement.ElementAlignment.right;
						}

						columnFooter = Renderer.addElement(columnFooter, ReportElement.ElementType.textField, a.getName(), valueExpression, null, null, new Integer(0),
								new Integer(0),
								new Integer(0), null, null, alignment, design.getBoldLabels(), null, null, null, null);
					}
				}
				columnFooter.spreadElements();
			}
			design.getBands().add(columnFooter);
		} else if (DefinitionSource.view.equals(design.getDefinitionSource())) {
			design.getBands().addAll(visitor.getDetailBands());
		}

		// page numbers
		if (Boolean.TRUE.equals(design.getIncludePageNumbers())) {
			ReportBand pageFooter = new ReportBand();
			pageFooter.setBandType(ReportBand.BandType.pageFooter);
			pageFooter.setParent(design);

			if (ReportType.report.equals(design.getReportType())) {

				// page X of Y
				Integer width = new Integer(design.getColumnWidth().intValue() - design.getDefaultElementHeight().intValue());
				Integer left = new Integer(0);
				pageFooter = Renderer.addElement(pageFooter, ReportElement.ElementType.textField, "pageX", "\"Page \" + $V{PAGE_NUMBER} + \" of \"", null, null, new Integer(0),
						left,
						width, null, null, ReportElement.ElementAlignment.right, null, null, null, null, null);

				width = design.getDefaultElementHeight();
				left = new Integer(design.getColumnWidth().intValue() - design.getDefaultElementHeight().intValue());
				pageFooter = Renderer.addElement(pageFooter, ReportElement.ElementType.textField, "pageOfY", "$V{PAGE_NUMBER}", null, null, new Integer(0), left, width, null, null,
						null, null, null, null, null, null);
				pageFooter.getElements().get(1).setEvaluationTime(ReportElement.EvaluationTime.report);
			}

			design.getBands().add(pageFooter);
		}

		ReportBand lastPageFooter = new ReportBand();
		lastPageFooter.setBandType(ReportBand.BandType.lastPageFooter);
		lastPageFooter.setParent(design);
		design.getBands().add(lastPageFooter);

		ReportBand summary = new ReportBand();
		summary.setBandType(ReportBand.BandType.summary);
		summary.setParent(design);
		design.getBands().add(summary);

		// no data section
		ReportBand noData = new ReportBand();
		noData.setBandType(ReportBand.BandType.noData);
		noData.setParent(design);

		noData = Renderer.addElement(noData, ReportElement.ElementType.staticText, "no_data_title", "No data matched your selection.", null, null, new Integer(0), new Integer(0),
				design.getColumnWidth(),
				design.getDefaultElementHeight() * 2, null, null, Boolean.TRUE, null, null, null, null);

		if (Boolean.TRUE.equals(design.getSectionBorderTop())
				|| Boolean.TRUE.equals(design.getSectionBorderLeft())
				|| Boolean.TRUE.equals(design.getSectionBorderBottom())
				|| Boolean.TRUE.equals(design.getSectionBorderRight())) {

			// TODO implement borders as lines
			noData = Renderer.addElement(noData, ReportElement.ElementType.border, null, null, null, null, new Integer(0), new Integer(0), design.getColumnWidth(),
					design.getDefaultElementHeight() * 2, null, null, null, null, null, null, null);
		}

		design.getBands().add(noData);
	}

	/**
	 * Construct a subreport for the field which represents the collection
	 * 
	 * @param fld
	 * @return
	 */
	public void constructSubreportFromField(ReportField fld, Integer colWidth)
			throws Exception {

		DesignSpecification subreport = new DesignSpecification();
		subreport.setReportType(DesignSpecification.ReportType.subreport);
		subreport.setName(design.getName() + "_" + fld.getName());
		subreport.setModuleName(fld.getOwningModuleName());
		subreport.setDocumentName(fld.getDocumentName());
		subreport.setUxui(design.getUxui());

		subreport.setWidth(colWidth);
		subreport.setColumnWidth(colWidth);
		subreport.setLeftMargin(new Integer(0)); // subreports normally don't have a margin as they sit inside another report
		subreport.setRightMargin(new Integer(0)); // padding around subreports can be handled within the containing report
		subreport.setTopMargin(new Integer(0));
		subreport.setBottomMargin(new Integer(0));

		subreport.setMode(design.getMode()); // default to same mode as containing report
		subreport.setField(fld);
		subreport.setParentReportPersistentName(Renderer.getPersistentFromDocument(document));
		subreport.setRepositoryPath(design.getRepositoryPath());

		// get the attribute and collection type
		for (Attribute attr : document.getAttributes()) {
			if (attr.getName().equals(fld.getName())) {
				Collection col = (Collection) attr;
				subreport.setCollectionType(col.getType());
				break;
			}
		}

		Generator subReportGenerator = new Generator(subreport);
		subReportGenerator.generateDefaultDesign();

		design.getSubReports().add(subreport);
	}
}
