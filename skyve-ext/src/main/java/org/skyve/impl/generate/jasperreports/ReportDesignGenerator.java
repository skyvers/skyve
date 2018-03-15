package org.skyve.impl.generate.jasperreports;

import org.elasticsearch.common.Preconditions;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;

import java.util.HashMap;

public abstract class ReportDesignGenerator {

    public DesignSpecification generateDesign() {
        return populateDesign(new DesignSpecification());
    }

    public DesignSpecification populateDesign(DesignSpecification design) {
        Preconditions.checkArgument(design.getModuleName() != null);
        Preconditions.checkArgument(design.getDocumentName() != null);

        design.setJoins(new HashMap<>());
        design.setJoinAlias(new HashMap<>());
        design.setAlias((int) ('a'));

        clearDesign(design);

        addParameters(design);
        addFields(design);
        addVariables(design);
        addSubreports(design);
        addBands(design);

        return design;
    }

    protected abstract ReportDesignGenerator getSubreportGenerator();

    private void clearDesign(DesignSpecification design) {
        design.getBands().clear();
        design.getFields().clear();
        design.getParameters().clear();
        design.getVariables().clear();
        design.getSubReports().clear();
    }

    protected void addParameters(DesignSpecification design) {
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

    protected void addFields(DesignSpecification design) {
        final Customer customer = design.getCustomer();
        final Module module = design.getModule();
        final Document document = design.getDocument();
        if (DesignSpecification.ReportType.report.equals(design.getReportType())) {
            if (DesignSpecification.Mode.bean.equals(design.getMode())) {
                ReportField fThis = new ReportField();
                fThis.setParent(design);
                fThis.setName("THIS");
                fThis.setTypeClass("modules." + module.getName() + ".domain." + document.getName());
                fThis.setImplicit(Boolean.TRUE);
                design.getFields().add(fThis);

                ReportField fUser = new ReportField();
                fUser.setName("USER");
                fUser.setParent(design);
                fUser.setTypeClass("org.skyve.metadata.user.User");
                fUser.setImplicit(Boolean.TRUE);
                design.getFields().add(fUser);
            }

            ReportField fbK = new ReportField();
            fbK.setName("bizKey");
            fbK.setTypeClass("java.lang.String");
            fbK.setImplicit(Boolean.TRUE);

            Document extDocument = null;
            if (document.getExtends() != null && document.getExtends().getDocumentName() != null && Persistent.ExtensionStrategy.joined.equals(document.getPersistent().getStrategy())) {
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

            design.getFields().add(fbK);
        }
    }

    protected void addVariables(DesignSpecification design) {
        if (DesignSpecification.ReportType.subreport.equals(design.getReportType())) {
            for (ReportField a : design.getFields()) {
                if (!Attribute.AttributeType.collection.name().equals(a.getSkyveType()) && !Boolean.TRUE.equals(a.getImplicit())) {
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

    protected void addSubreports(DesignSpecification design) {
        //only generate default reports if subreports is empty (allows for manual creation of subreports instead of default)
        if (design.getSubReports().isEmpty()) {

            for (ReportField a : design.getFields()) {
                if (DesignSpecification.ReportType.report.equals(design.getReportType())
                        && Attribute.AttributeType.collection.name().equals(a.getSkyveType())) {

                    constructSubreportFromField(design, a, design.getColumnWidth());

                }
            }
        }
    }

    protected void addBands(DesignSpecification design) {
        ReportBand background = new ReportBand();
        background.setBandType(ReportBand.BandType.background);
        background.setParent(design);
        design.getBands().add(background);

        ReportBand pageHeader = new ReportBand();
        pageHeader.setBandType(ReportBand.BandType.pageHeader);
        pageHeader.setParent(design);
        design.getBands().add(pageHeader);

        // page numbers
        if (Boolean.TRUE.equals(design.getIncludePageNumbers())) {
            ReportBand pageFooter = new ReportBand();
            pageFooter.setBandType(ReportBand.BandType.pageFooter);
            pageFooter.setParent(design);

            if (DesignSpecification.ReportType.report.equals(design.getReportType())) {

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

    protected ReportField fieldFromAttribute(DesignSpecification bean, Customer customer, Document document, Attribute a, StringBuilder sJoin, StringBuilder fieldPrefix) {
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

                if (DesignSpecification.Mode.bean.equals(bean.getMode())) {
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

                if (DesignSpecification.Mode.bean.equals(bean.getMode())) {

                    f.setTypeClass("java.lang.String"); // Binder will convert to String
                } else {
                    f.setTypeClass(Renderer.getSqlEquivalentClass(a.getAttributeType()));
                }

                if (Attribute.AttributeType.decimal2.equals(a.getAttributeType()) || Attribute.AttributeType.decimal5.equals(a.getAttributeType())
                        || Attribute.AttributeType.decimal10.equals(a.getAttributeType())
                        || Attribute.AttributeType.integer.equals(a.getAttributeType())
                        || Attribute.AttributeType.longInteger.equals(a.getAttributeType())) {
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
    protected ReportField fieldFromBinding(DesignSpecification design, Customer customer, Document document, String binding) {

        ReportField result = null;
        StringBuilder sJoin = new StringBuilder();
        StringBuilder prefix = new StringBuilder();
        try {

            if (binding.contains(".")) {

                String[] bindings = binding.split("\\.");
                String uniqueJoinIdentifier = "";
                for (int i = 0; i < bindings.length; i++) {
                    Attribute a = document.getAttribute(bindings[i]);

                    if (DesignSpecification.Mode.sql.equals(design.getMode()) && !a.isPersistent()) {
                        // abandon creating a field - sql reports can't report on non persistent fields
                        break;
                    } else {

                        // build up join string
                        if (Attribute.AttributeType.association.equals(a.getAttributeType())) {
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
                if (DesignSpecification.Mode.sql.equals(design.getMode())
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

    protected StringBuilder addJoinForAssociation(StringBuilder sJoin, Customer customer, DesignSpecification bean,
                                                         Document document, Attribute a, String uniqueJoinIdentifier) {

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
     * Construct a subreport for the field which represents the collection
     *
     * @param fld
     * @return
     */
    public void constructSubreportFromField(DesignSpecification design, ReportField fld, Integer colWidth) {
        final Document document = design.getDocument();

        DesignSpecification subreport = new DesignSpecification();
        subreport.setReportType(DesignSpecification.ReportType.subreport);
        subreport.setName(design.getName() + "_" + fld.getName());
        subreport.setModuleName(fld.getOwningModuleName());
        subreport.setDocumentName(fld.getDocumentName());
        subreport.setUxui(design.getUxui());

        subreport.setWidth(colWidth);
        subreport.setColumnWidth(colWidth);
        subreport.setLeftMargin(0); // subreports normally don't have a margin as they sit inside another report
        subreport.setRightMargin(0); // padding around subreports can be handled within the containing report
        subreport.setTopMargin(0);
        subreport.setBottomMargin(0);

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

        getSubreportGenerator().populateDesign(subreport);

        design.getSubReports().add(subreport);
    }

}
