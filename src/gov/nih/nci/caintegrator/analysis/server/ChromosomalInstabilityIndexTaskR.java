package gov.nih.nci.caintegrator.analysis.server;


import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import gov.nih.nci.caintegrator.analysis.messaging.*;
import gov.nih.nci.caintegrator.exceptions.AnalysisServerException;

import org.apache.log4j.Logger;
import org.rosuda.REngine.REXP;
import org.rosuda.REngine.Rserve.RFileInputStream;


/**
 * 
 * Performs Chromosomal Instability Index using R.
 * 
 * @author Lavinia A. Carabet
 *
 *
 */


/**
* caIntegrator License
* 
* Copyright 2001-2005 Science Applications International Corporation ("SAIC"). 
* The software subject to this notice and license includes both human readable source code form and machine readable, 
* binary, object code form ("the caIntegrator Software"). The caIntegrator Software was developed in conjunction with 
* the National Cancer Institute ("NCI") by NCI employees and employees of SAIC. 
* To the extent government employees are authors, any rights in such works shall be subject to Title 17 of the United States
* Code, section 105. 
* This caIntegrator Software License (the "License") is between NCI and You. "You (or "Your") shall mean a person or an 
* entity, and all other entities that control, are controlled by, or are under common control with the entity. "Control" 
* for purposes of this definition means (i) the direct or indirect power to cause the direction or management of such entity,
*  whether by contract or otherwise, or (ii) ownership of fifty percent (50%) or more of the outstanding shares, or (iii) 
* beneficial ownership of such entity. 
* This License is granted provided that You agree to the conditions described below. NCI grants You a non-exclusive, 
* worldwide, perpetual, fully-paid-up, no-charge, irrevocable, transferable and royalty-free right and license in its rights 
* in the caIntegrator Software to (i) use, install, access, operate, execute, copy, modify, translate, market, publicly 
* display, publicly perform, and prepare derivative works of the caIntegrator Software; (ii) distribute and have distributed 
* to and by third parties the caIntegrator Software and any modifications and derivative works thereof; 
* and (iii) sublicense the foregoing rights set out in (i) and (ii) to third parties, including the right to license such 
* rights to further third parties. For sake of clarity, and not by way of limitation, NCI shall have no right of accounting
* or right of payment from You or Your sublicensees for the rights granted under this License. This License is granted at no
* charge to You. 
* 1. Your redistributions of the source code for the Software must retain the above copyright notice, this list of conditions
*    and the disclaimer and limitation of liability of Article 6, below. Your redistributions in object code form must reproduce 
*    the above copyright notice, this list of conditions and the disclaimer of Article 6 in the documentation and/or other materials
*    provided with the distribution, if any. 
* 2. Your end-user documentation included with the redistribution, if any, must include the following acknowledgment: "This 
*    product includes software developed by SAIC and the National Cancer Institute." If You do not include such end-user 
*    documentation, You shall include this acknowledgment in the Software itself, wherever such third-party acknowledgments 
*    normally appear.
* 3. You may not use the names "The National Cancer Institute", "NCI" "Science Applications International Corporation" and 
*    "SAIC" to endorse or promote products derived from this Software. This License does not authorize You to use any 
*    trademarks, service marks, trade names, logos or product names of either NCI or SAIC, except as required to comply with
*    the terms of this License. 
* 4. For sake of clarity, and not by way of limitation, You may incorporate this Software into Your proprietary programs and 
*    into any third party proprietary programs. However, if You incorporate the Software into third party proprietary 
*    programs, You agree that You are solely responsible for obtaining any permission from such third parties required to 
*    incorporate the Software into such third party proprietary programs and for informing Your sublicensees, including 
*    without limitation Your end-users, of their obligation to secure any required permissions from such third parties 
*    before incorporating the Software into such third party proprietary software programs. In the event that You fail 
*    to obtain such permissions, You agree to indemnify NCI for any claims against NCI by such third parties, except to 
*    the extent prohibited by law, resulting from Your failure to obtain such permissions. 
* 5. For sake of clarity, and not by way of limitation, You may add Your own copyright statement to Your modifications and 
*    to the derivative works, and You may provide additional or different license terms and conditions in Your sublicenses 
*    of modifications of the Software, or any derivative works of the Software as a whole, provided Your use, reproduction, 
*    and distribution of the Work otherwise complies with the conditions stated in this License.
* 6. THIS SOFTWARE IS PROVIDED "AS IS," AND ANY EXPRESSED OR IMPLIED WARRANTIES, (INCLUDING, BUT NOT LIMITED TO, 
*    THE IMPLIED WARRANTIES OF MERCHANTABILITY, NON-INFRINGEMENT AND FITNESS FOR A PARTICULAR PURPOSE) ARE DISCLAIMED. 
*    IN NO EVENT SHALL THE NATIONAL CANCER INSTITUTE, SAIC, OR THEIR AFFILIATES BE LIABLE FOR ANY DIRECT, INDIRECT, 
*    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
*    GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
*    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
*    OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
* 
*/

public class ChromosomalInstabilityIndexTaskR extends AnalysisTaskR {

	private ChromosomalInstabilityIndexResult cinResult = null;
	public static final int MIN_GROUP_SIZE = 3;
	private static Logger logger = Logger.getLogger(ChromosomalInstabilityIndexTaskR.class);

	public ChromosomalInstabilityIndexTaskR(
			ChromosomalInstabilityIndexRequest request) {
		this(request, false);
	}

	public ChromosomalInstabilityIndexTaskR(
			ChromosomalInstabilityIndexRequest request, boolean debugRcommands) {
		super(request, debugRcommands);
	}

	public void run() {
		ChromosomalInstabilityIndexRequest request = (ChromosomalInstabilityIndexRequest)getRequest();
		SampleGroup sampleGroup1 = request.getGroup1();
		SampleGroup sampleGroup2 = request.getGroup2();
		
		try {
			
			String dataFileName = request.getDataFileName();
			if (dataFileName != null) {
			  setDataFile(request.getDataFileName());
			}
			else {
			  throw new AnalysisServerException("Null cin data file name");
			}
					
			if ((sampleGroup1 == null) || (sampleGroup1.size() < MIN_GROUP_SIZE)) {
				  AnalysisServerException ex = new AnalysisServerException(
					"Group1 is null or has less than " + MIN_GROUP_SIZE + " entries.");		 
			      ex.setFailedRequest(request);
			      setException(ex);
			      logger.error(ex.getMessage());
			      return;
			}
					
			if ((sampleGroup2 == null) || (sampleGroup2.size() < MIN_GROUP_SIZE)) {
				  AnalysisServerException ex = new AnalysisServerException(
					"Group2 is null or has less than " + MIN_GROUP_SIZE + " entries.");		 
			      ex.setFailedRequest(request);
			      setException(ex);
			      logger.error(ex.getMessage());
			      return;
			}
					
			//check to see if there are any overlapping samples between the two groups
			if ((sampleGroup1 != null)&&(sampleGroup2 != null)) {
			  
			  //get overlap between the two sets
			  Set<String> intersection = new HashSet<String>();
			  intersection.addAll(sampleGroup1);
			  intersection.retainAll(sampleGroup2);
			  
			  if (intersection.size() > 0) {
			     //the groups are overlapping so return an exception
				 StringBuffer ids = new StringBuffer();
				 for (Iterator i=intersection.iterator(); i.hasNext(); ) {
				   ids.append(i.next());
				   if (i.hasNext()) {
				     ids.append(",");
				   }
				 }
				 
				 AnalysisServerException ex = new AnalysisServerException(
					      "Can not perform cin with overlapping groups. Overlapping ids=" + ids.toString());		 
					      ex.setFailedRequest(request);
					      setException(ex);
					      logger.error(ex.getMessage());
					      return;   
			  }
			}
			
			//For now assume that there are two groups. When we get data for two channel array then
			//allow only one group so leaving in the possibility of having only one group in the code 
			//below even though the one group case won't be executed because of the tests above.
			
			
			int sampleGrp1Len = 0, sampleGrp2Len = 0;
			
			sampleGrp1Len = sampleGroup1.size();
			
			String sampleGrp1RName = sampleGroup1.getGroupName().replaceAll(" ", "_");
			String sampleGrp2RName = sampleGroup2.getGroupName().replaceAll(" ", "_");
			
			String rCmd = null;
		
			rCmd = getRgroupCmd(sampleGrp1RName, sampleGroup1);
	
			doRvoidEval(rCmd);
		
			if (sampleGroup2 != null) {
				// two group comparison
				sampleGrp2Len = sampleGroup2.size();
					
				rCmd = getRgroupCmd(sampleGrp2RName, sampleGroup2);
				doRvoidEval(rCmd);
		
				// create the input data matrix using the sample groups
				rCmd = "cinInputMatrix <- getSubmatrix.twogrps(dataMatrix,"
						+ sampleGrp2RName + "," + sampleGrp1RName + ")";
				doRvoidEval(rCmd);
		
				// check to make sure all identifiers matched in the R data file
				rCmd = "dim(cinInputMatrix)[2]";
				int numMatched = doREval(rCmd).asInteger();
				if (numMatched != (sampleGrp1Len + sampleGrp2Len)) {
					AnalysisServerException ex = new AnalysisServerException(
							"Some sample ids did not match R data file for cin request.");
					ex.setFailedRequest(request);
					setException(ex);
					logger.error(ex.getMessage());
					return;
				}
			} else {
				// single group comparison
//				baselineGrpLen = 0;
//				rCmd = "cinInputMatrix <- getSubmatrix.onegrp(dataMatrix,"
//						+ grp1RName + ")";
//				doRvoidEval(rCmd);
				logger.error("Single group cin is not currently supported.");
				throw new AnalysisServerException("Unsupported operation: Attempted to do a single group cin.");
			}
			
			cinResult = new ChromosomalInstabilityIndexResult(getRequest().getSessionId(), getRequest().getTaskId());
			String baseFileName = "cin_" + getRequest().getSessionId() + "_" + System.currentTimeMillis();	
			
			String heatMapFileName = "heatMap_" + baseFileName;
			rCmd = "clinical.inf <- as.matrix( cbind( dimnames(cinInputMatrix)[[2]], c( rep(" + "'" + sampleGrp2RName + "'," + sampleGrp2Len +"), rep(" + "'" + sampleGrp1RName + "'," + sampleGrp1Len +") ) ) )";
			logger.debug(rCmd);
			doRvoidEval(rCmd);
			rCmd = "heatmap.draw(cinInputMatrix, clinical.inf, heatmap.title=" + "'" + heatMapFileName + "'" +")";
			doRvoidEval(rCmd);
			
			RFileInputStream is = getRComputeConnection().openFile(heatMapFileName + ".png");
			byte[] heatMapFile = getBytes(is);
			is.close();
			
			cinResult.addCinFiles("heatmap", heatMapFile);
			getRComputeConnection().removeFile(heatMapFileName + ".png");
			
			dataFileName = request.getCytobandsDataFileName();
			if (dataFileName != null) {
			  setDataFile(request.getCytobandsDataFileName());
			}
			else {
			  throw new AnalysisServerException("Null cin cytobands data file name");
			}
			
			doRvoidEval("cinInputMatrix <- dataMatrix");
			setDataFile("hg18_annot.Rda");
			doRvoidEval("annotInputMatrix <- hg18_annot");
			
			String cytobandsFileName = null;
			
			for (int i = 1; i < 23; i++) {
				cytobandsFileName = "chr" + i + "_cytobands_" + baseFileName;
				rCmd = "cytobands_cin.draw(cinInputMatrix, clinical.inf, " + i + ", annotInputMatrix, title_text=" + "'" + cytobandsFileName + "'" +")";
				doRvoidEval(rCmd);
				
				is = getRComputeConnection().openFile(cytobandsFileName + ".png");
				byte[] cytobandsFile = getBytes(is);
				is.close();
				
				cinResult.addCinFiles(i+"", cytobandsFile);
				getRComputeConnection().removeFile(cytobandsFileName + ".png");
			}
			
		} catch (AnalysisServerException e) {
			e.setFailedRequest(request);
			logger.error("Internal Error. " + e.getMessage());
			logStackTrace(logger, e);
			setException(e);
			return;
		} catch (Exception ex) {
			AnalysisServerException asex = new AnalysisServerException(
					"Internal Error. Caught AnalysisServerException in ChromosomalInstabilityIndexTaskR." + ex.getMessage());
			asex.setFailedRequest(request);
			setException(asex);	        
			logStackTrace(logger, ex);
			return;  
		}

	}

	@Override
	public AnalysisResult getResult() {
		return cinResult;
	}

	/**
	 * Clean up some R memory and release and remove the 
	 * reference to the R connection so that this task can be 
	 * garbage collected.
	 */
	public void cleanUp() {
		try {
			setRComputeConnection(null);
		} catch (AnalysisServerException e) {
		   logger.error("Error in cleanUp method");
		   logStackTrace(logger, e);
		   setException(e);
		}
	}

}

