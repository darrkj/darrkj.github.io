# Zombie Spread

## Introduction

Put some intro here

## Data Model


//hide

//setup
[source, cypher]
----
  CREATE (Alabama:State{name:"Alabama"})                          
CREATE (Arizona:State{name:"Arizona"})                          
CREATE (Arkansas:State{name:"Arkansas"})                        
CREATE (California:State{name:"California"})                    
CREATE (Colorado:State{name:"Colorado"})                        
CREATE (Connecticut:State{name:"Connecticut"})                  
CREATE (Delaware:State{name:"Delaware"})                        
CREATE (District_of_Columbia:State{name:"District of Columbia"})
CREATE (Florida:State{name:"Florida"})                          
CREATE (Georgia:State{name:"Georgia"})                          
CREATE (Idaho:State{name:"Idaho"})                              
CREATE (Illinois:State{name:"Illinois"})                        
CREATE (Indiana:State{name:"Indiana"})                          
CREATE (Iowa:State{name:"Iowa"})                                
CREATE (Kansas:State{name:"Kansas"})                            
CREATE (Kentucky:State{name:"Kentucky"})                        
CREATE (Louisiana:State{name:"Louisiana"})                      
CREATE (Maine:State{name:"Maine"})                              
CREATE (Maryland:State{name:"Maryland"})                        
CREATE (Massachusetts:State{name:"Massachusetts"})              
CREATE (Michigan:State{name:"Michigan"})                        
CREATE (Minnesota:State{name:"Minnesota"})                      
CREATE (Mississippi:State{name:"Mississippi"})                  
CREATE (Missouri:State{name:"Missouri"})                        
CREATE (Montana:State{name:"Montana"})                          
CREATE (Nebraska:State{name:"Nebraska"})                        
CREATE (Nevada:State{name:"Nevada"})                            
CREATE (New_Hampshire:State{name:"New Hampshire"})              
CREATE (New_Jersey:State{name:"New Jersey"})                    
CREATE (New_Mexico:State{name:"New Mexico"})                    
CREATE (New_York:State{name:"New York"})                        
CREATE (North_Carolina:State{name:"North Carolina"})            
CREATE (North_Dakota:State{name:"North Dakota"})                
CREATE (Ohio:State{name:"Ohio"})                                
CREATE (Oklahoma:State{name:"Oklahoma"})                        
CREATE (Oregon:State{name:"Oregon"})                            
CREATE (Pennsylvania:State{name:"Pennsylvania"})                
CREATE (Rhode_Island:State{name:"Rhode Island"})                
CREATE (South_Carolina:State{name:"South Carolina"})            
CREATE (South_Dakota:State{name:"South Dakota"})                
CREATE (Tennessee:State{name:"Tennessee"})                      
CREATE (Texas:State{name:"Texas"})                              
CREATE (Utah:State{name:"Utah"})                                
CREATE (Vermont:State{name:"Vermont"})                          
CREATE (Virginia:State{name:"Virginia"})                        
CREATE (Washington:State{name:"Washington"})                    
CREATE (West_Virginia:State{name:"West Virginia"})              
CREATE (Wisconsin:State{name:"Wisconsin"})                      
CREATE (Wyoming:State{name:"Wyoming"}) 




CREATE
(Alabama)-[:BORDERS]->(Mississippi),          
(Alabama)-[:BORDERS]->(Tennessee),            
(Alabama)-[:BORDERS]->(Georgia),              
(Alabama)-[:BORDERS]->(Florida),              
(Arizona)-[:BORDERS]->(California),           
(Arizona)-[:BORDERS]->(Nevada),               
(Arizona)-[:BORDERS]->(Utah),                 
(Arizona)-[:BORDERS]->(New_Mexico),           
(Arkansas)-[:BORDERS]->(Missouri),            
(Arkansas)-[:BORDERS]->(Tennessee),           
(Arkansas)-[:BORDERS]->(Mississippi),         
(Arkansas)-[:BORDERS]->(Louisiana),           
(Arkansas)-[:BORDERS]->(Texas),               
(Arkansas)-[:BORDERS]->(Oklahoma),            
(California)-[:BORDERS]->(Oregon),            
(California)-[:BORDERS]->(Nevada),            
(California)-[:BORDERS]->(Arizona),           
(Colorado)-[:BORDERS]->(Wyoming),             
(Colorado)-[:BORDERS]->(Nebraska),            
(Colorado)-[:BORDERS]->(Kansas),              
(Colorado)-[:BORDERS]->(Oklahoma),            
(Colorado)-[:BORDERS]->(New_Mexico),          
(Colorado)-[:BORDERS]->(Utah),                
(Connecticut)-[:BORDERS]->(New_York),         
(Connecticut)-[:BORDERS]->(Massachusetts),    
(Connecticut)-[:BORDERS]->(Rhode_Island),     
(Delaware)-[:BORDERS]->(Pennsylvania),        
(Delaware)-[:BORDERS]->(New_Jersey),          
(Delaware)-[:BORDERS]->(Maryland),            
(District_of_Columbia)-[:BORDERS]->(Virginia),
(District_of_Columbia)-[:BORDERS]->(Maryland),
(Florida)-[:BORDERS]->(Georgia),              
(Florida)-[:BORDERS]->(Alabama),              
(Georgia)-[:BORDERS]->(Alabama),              
(Georgia)-[:BORDERS]->(Florida),              
(Georgia)-[:BORDERS]->(South_Carolina),       
(Georgia)-[:BORDERS]->(North_Carolina),       
(Georgia)-[:BORDERS]->(Tennessee),            
(Idaho)-[:BORDERS]->(Montana),                
(Idaho)-[:BORDERS]->(Wyoming),                
(Idaho)-[:BORDERS]->(Utah),                   
(Idaho)-[:BORDERS]->(Nevada),                 
(Idaho)-[:BORDERS]->(Oregon),                 
(Idaho)-[:BORDERS]->(Washington),             
(Illinois)-[:BORDERS]->(Iowa),                
(Illinois)-[:BORDERS]->(Wisconsin),           
(Illinois)-[:BORDERS]->(Indiana),             
(Illinois)-[:BORDERS]->(Kentucky),            
(Illinois)-[:BORDERS]->(Missouri),            
(Indiana)-[:BORDERS]->(Michigan),             
(Indiana)-[:BORDERS]->(Ohio),                 
(Indiana)-[:BORDERS]->(Kentucky),             
(Indiana)-[:BORDERS]->(Illinois),             
(Iowa)-[:BORDERS]->(Minnesota),               
(Iowa)-[:BORDERS]->(Wisconsin),               
(Iowa)-[:BORDERS]->(Illinois),                
(Iowa)-[:BORDERS]->(Missouri),                
(Iowa)-[:BORDERS]->(Nebraska),                
(Iowa)-[:BORDERS]->(South_Dakota),            
(Kansas)-[:BORDERS]->(Nebraska),              
(Kansas)-[:BORDERS]->(Missouri),              
(Kansas)-[:BORDERS]->(Oklahoma),              
(Kansas)-[:BORDERS]->(Colorado),              
(Kentucky)-[:BORDERS]->(Missouri),            
(Kentucky)-[:BORDERS]->(Illinois),            
(Kentucky)-[:BORDERS]->(Indiana),             
(Kentucky)-[:BORDERS]->(Ohio),                
(Kentucky)-[:BORDERS]->(West_Virginia),       
(Kentucky)-[:BORDERS]->(Virginia),            
(Kentucky)-[:BORDERS]->(Tennessee),           
(Louisiana)-[:BORDERS]->(Texas),              
(Louisiana)-[:BORDERS]->(Arkansas),           
(Louisiana)-[:BORDERS]->(Mississippi),        
(Maine)-[:BORDERS]->(New_Hampshire),          
(Maryland)-[:BORDERS]->(West_Virginia),        
(Maryland)-[:BORDERS]->(Virginia),            
(Maryland)-[:BORDERS]->(Delaware),            
(Maryland)-[:BORDERS]->(Pennsylvania),        
(Maryland)-[:BORDERS]->(District_of_Columbia),
(Massachusetts)-[:BORDERS]->(Connecticut),    
(Massachusetts)-[:BORDERS]->(Rhode_Island),   
(Massachusetts)-[:BORDERS]->(New_Hampshire),  
(Massachusetts)-[:BORDERS]->(Vermont),        
(Massachusetts)-[:BORDERS]->(New_York),       
(Michigan)-[:BORDERS]->(Wisconsin),           
(Michigan)-[:BORDERS]->(Indiana),             
(Michigan)-[:BORDERS]->(Ohio),                
(Minnesota)-[:BORDERS]->(North_Dakota),       
(Minnesota)-[:BORDERS]->(South_Dakota),       
(Minnesota)-[:BORDERS]->(Iowa),               
(Minnesota)-[:BORDERS]->(Wisconsin),          
(Mississippi)-[:BORDERS]->(Tennessee),        
(Mississippi)-[:BORDERS]->(Alabama),          
(Mississippi)-[:BORDERS]->(Louisiana),        
(Mississippi)-[:BORDERS]->(Arkansas),         
(Missouri)-[:BORDERS]->(Iowa),                
(Missouri)-[:BORDERS]->(Illinois),            
(Missouri)-[:BORDERS]->(Kentucky),            
(Missouri)-[:BORDERS]->(Tennessee),           
(Missouri)-[:BORDERS]->(Arkansas),            
(Missouri)-[:BORDERS]->(Oklahoma),            
(Missouri)-[:BORDERS]->(Kansas),              
(Missouri)-[:BORDERS]->(Nebraska),            
(Montana)-[:BORDERS]->(North_Dakota),         
(Montana)-[:BORDERS]->(South_Dakota),         
(Montana)-[:BORDERS]->(Wyoming),              
(Montana)-[:BORDERS]->(Idaho),                
(Nebraska)-[:BORDERS]->(South_Dakota),        
(Nebraska)-[:BORDERS]->(Iowa),                
(Nebraska)-[:BORDERS]->(Missouri),            
(Nebraska)-[:BORDERS]->(Kansas),              
(Nebraska)-[:BORDERS]->(Colorado),            
(Nebraska)-[:BORDERS]->(Wyoming),             
(Nevada)-[:BORDERS]->(Oregon),                
(Nevada)-[:BORDERS]->(Idaho),                 
(Nevada)-[:BORDERS]->(Utah),                  
(Nevada)-[:BORDERS]->(Arizona),               
(Nevada)-[:BORDERS]->(California),            
(New_Hampshire)-[:BORDERS]->(Maine),          
(New_Hampshire)-[:BORDERS]->(Massachusetts),  
(New_Hampshire)-[:BORDERS]->(Vermont),        
(New_Jersey)-[:BORDERS]->(New_York),          
(New_Jersey)-[:BORDERS]->(Pennsylvania),      
(New_Jersey)-[:BORDERS]->(Delaware),          
(New_Mexico)-[:BORDERS]->(Colorado),          
(New_Mexico)-[:BORDERS]->(Oklahoma),          
(New_Mexico)-[:BORDERS]->(Texas),             
(New_Mexico)-[:BORDERS]->(Arizona),           
(New_York)-[:BORDERS]->(Vermont),             
(New_York)-[:BORDERS]->(Massachusetts),       
(New_York)-[:BORDERS]->(New_Jersey),          
(New_York)-[:BORDERS]->(Pennsylvania),        
(New_York)-[:BORDERS]->(Connecticut),         
(North_Carolina)-[:BORDERS]->(Virginia),      
(North_Carolina)-[:BORDERS]->(Tennessee),     
(North_Carolina)-[:BORDERS]->(Georgia),       
(North_Carolina)-[:BORDERS]->(South_Carolina),
(North_Dakota)-[:BORDERS]->(Minnesota),       
(North_Dakota)-[:BORDERS]->(South_Dakota),    
(North_Dakota)-[:BORDERS]->(Montana),         
(Ohio)-[:BORDERS]->(Pennsylvania),            
(Ohio)-[:BORDERS]->(West_Virginia),           
(Ohio)-[:BORDERS]->(Kentucky),                
(Ohio)-[:BORDERS]->(Indiana),                 
(Ohio)-[:BORDERS]->(Michigan),                
(Oklahoma)-[:BORDERS]->(Kansas),              
(Oklahoma)-[:BORDERS]->(Missouri),            
(Oklahoma)-[:BORDERS]->(Arkansas),            
(Oklahoma)-[:BORDERS]->(Texas),               
(Oklahoma)-[:BORDERS]->(New_Mexico),          
(Oklahoma)-[:BORDERS]->(Colorado),            
(Oregon)-[:BORDERS]->(Washington),            
(Oregon)-[:BORDERS]->(Idaho),                 
(Oregon)-[:BORDERS]->(Nevada),                
(Oregon)-[:BORDERS]->(California),            
(Pennsylvania)-[:BORDERS]->(New_York),        
(Pennsylvania)-[:BORDERS]->(New_Jersey),      
(Pennsylvania)-[:BORDERS]->(Maryland),        
(Pennsylvania)-[:BORDERS]->(Delaware),        
(Pennsylvania)-[:BORDERS]->(West_Virginia),   
(Pennsylvania)-[:BORDERS]->(Ohio),            
(Rhode_Island)-[:BORDERS]->(Massachusetts),   
(Rhode_Island)-[:BORDERS]->(Connecticut),     
(South_Carolina)-[:BORDERS]->(Georgia),       
(South_Carolina)-[:BORDERS]->(North_Carolina),
(South_Dakota)-[:BORDERS]->(North_Dakota),    
(South_Dakota)-[:BORDERS]->(Minnesota),       
(South_Dakota)-[:BORDERS]->(Iowa),            
(South_Dakota)-[:BORDERS]->(Nebraska),        
(South_Dakota)-[:BORDERS]->(Wyoming),         
(South_Dakota)-[:BORDERS]->(Montana),         
(Tennessee)-[:BORDERS]->(North_Carolina),     
(Tennessee)-[:BORDERS]->(Georgia),            
(Tennessee)-[:BORDERS]->(Alabama),            
(Tennessee)-[:BORDERS]->(Mississippi),        
(Tennessee)-[:BORDERS]->(Arkansas),           
(Tennessee)-[:BORDERS]->(Missouri),           
(Tennessee)-[:BORDERS]->(Kentucky),           
(Tennessee)-[:BORDERS]->(Virginia),           
(Texas)-[:BORDERS]->(New_Mexico),             
(Texas)-[:BORDERS]->(Oklahoma),               
(Texas)-[:BORDERS]->(Arkansas),               
(Texas)-[:BORDERS]->(Louisiana),              
(Utah)-[:BORDERS]->(Idaho),                   
(Utah)-[:BORDERS]->(Wyoming),                 
(Utah)-[:BORDERS]->(Colorado),                
(Utah)-[:BORDERS]->(Arizona),                 
(Utah)-[:BORDERS]->(Nevada),                  
(Vermont)-[:BORDERS]->(New_Hampshire),        
(Vermont)-[:BORDERS]->(Massachusetts),        
(Vermont)-[:BORDERS]->(New_York),             
(Virginia)-[:BORDERS]->(North_Carolina),      
(Virginia)-[:BORDERS]->(Tennessee),           
(Virginia)-[:BORDERS]->(Kentucky),            
(Virginia)-[:BORDERS]->(West_Virginia),       
(Virginia)-[:BORDERS]->(Maryland),            
(Virginia)-[:BORDERS]->(District_of_Columbia),
(Washington)-[:BORDERS]->(Idaho),             
(Washington)-[:BORDERS]->(Oregon),            
(West_Virginia)-[:BORDERS]->(Pennsylvania),   
(West_Virginia)-[:BORDERS]->(Maryland),       
(West_Virginia)-[:BORDERS]->(Virginia),       
(West_Virginia)-[:BORDERS]->(Kentucky),       
(West_Virginia)-[:BORDERS]->(Ohio),           
(Wisconsin)-[:BORDERS]->(Minnesota),          
(Wisconsin)-[:BORDERS]->(Iowa),               
(Wisconsin)-[:BORDERS]->(Illinois),           
(Wisconsin)-[:BORDERS]->(Michigan),           
(Wyoming)-[:BORDERS]->(Montana),              
(Wyoming)-[:BORDERS]->(South_Dakota),         
(Wyoming)-[:BORDERS]->(Nebraska),             
(Wyoming)-[:BORDERS]->(Colorado),             
(Wyoming)-[:BORDERS]->(Utah),                 
(Wyoming)-[:BORDERS]->(Idaho)


----
  //graph

== Companies with greater production than sales
This query shows the companies that increased their car inventory in 2013, by making more cars than they could sell.
[source,cypher]
----
  MATCH n where n.name = 'Ohio' set n.infect = TRUE, n.spread = FALSE;
  
  MATCH (n)-[r]->(m) where n.infect = TRUE and n.spread = FALSE set r.contact = TRUE, n.spread = TRUE;
  
  match p = (n)-[r]->(m) where r.contact = TRUE foreach(i IN nodes(p) | set r.done = TRUE, m.infect = rand() > .5, m.spread = FALSE);
  
  
  match (n)-[r]->(m) where n.infect = TRUE and n.spread = FALSE and (m.infect is NULL or m.infect = FALSE) set r.contact = TRUE, n.spread = TRUE;
  
  
  match p = (n)-[r]->(m) where r.contact = TRUE and r.done is null foreach(i IN nodes(p) | set r.done = TRUE, m.infect = rand() > .5, m.spread = FALSE);
  
  
----
  //table
