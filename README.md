Texas Poker

An Erlang Server for The Muti-player Online Game Texas Poker

It Obeys OTP Principals, Inspired by Poker (https://github.com/wagerlabs/openpoker.git).

It is also a framework to build online games that divide players into small rooms. 

It is scalable depends on Erlang's distribution power.

I build it on aws.

Architecture:
  
  ELB --->  EC2 
      |           --> DynamoDB
      |-->  EC2
        
Tcp connections will be distributed in every EC2 by Elastic Load Balancer. 

And i take DynamoDB as a central database.

I make EC2s know each other by net_kernal:connect(),and 
every client is handled by a tcp process and a player process. 
The tcp process is local, while the player process is global. 
In this way ,i can easily manage situations like accidental
disconnection or repeating login.

I think it is quite fault tolerance. When one of ec2 breakdown ,only rooms in this machine break.
After telling the client this misfortune, the client reconnect to the other machine  and pray his loss 
fail its way to database.

If Erlang's distribution power is strong enough, i only need to add a same stuff to scale. Since every stuff
has the whole game logic, the operation job is so light.

My only headache is how many global process can be synchronized by an Erlang Cluster in this global way ?

global:register({glocal, user_id}, Pid). 

How far will it go before it is broken? 

Sorry for my English.


