# Notes on paper

Naor, P. (1969).
The regulation of queue size by levying tolls.
Econometrica: journal of the Econometric Society, 15-24.
<https://doi.org/10.2307/1909200>

------------------------------------------------------------------------

#### Disclaimers

These notes are a step leading to the final product and not the final product (a presentation is the final product).

Because of this:

-   These notes are not heavily proofread.
-   These notes are incomplete
-   Anything that makes me look bad was an accident/typo (and vice versa of course)

------------------------------------------------------------------------

## Terms

Not all directly related to the paper at hand.

-   "Utilization factor" - $\rho = \frac{\lambda}{\mu}$

    -   Ratio of arrival rate to service rate - how busy service station is

        -   e.g. 5 customers arrive per hour ( $\lambda=5$ ) and we can service 10 customers per hour ( $\mu=10$ ) $\to$ $\rho = \frac{5}{10} = 0.5$; 50% of service capacity is being utilized

-   "Generating function" - $g(z) = \sum_{n=0}^\infty p_iz^i$

    -   A generating function is a formal power series used to encode a sequence of numbers (such as $a0, a1, a2, ...$) in a compact mathematical form.

    -   In queuing models this can transform a sequence (like probability of different queue lengths) into functions that are easier to manipulate.
        Called a Probability Generating Function (PGF), when generating probs.

    -   Great, but long explanation of generating functions (3blue1brown on YouTube) - <https://www.youtube.com/watch?v=bOXCLR3Wric>

-   Not widely used terms today at all, used in papers cited by Naor: "obverse queue" and "reverse queue"

    -   "the obverse queue is the one formed in front of the counter by the customers, and the reverse queue is the one formed in back of the counter by the clerks" (This quote from - Brigham, G.(1955). On a Congestion Problem in an Aircraft Factory.
        *Journal of the Operations Research Society of America*, *3*(4), 412–428.
        <http://www.jstor.org/stable/166756>)

    -   "...higher prices on Saturday haircuts, which reduce the obverse queue of Saturday customers and the reverse queue of idle weekday barbers" (This quote from - Leeman, W. A. (1965).
        ‘Comments’ on Saaty’s ‘The Burdens of Queuing Charges’.
        Operations Research, 13(4), 680-681.
        <http://www.jstor.org/stable/167860>)

## Intro

-   Some say we should charge entry fee to services

    -   Related sources (cited in chronological order)

        -   Leeman, W. A.
            (1964).
            The reduction of queues through the use of price.
            Operations Research, 12(5), 783-785.
            <https://pubsonline.informs.org/doi/pdf/10.1287/opre.12.5.783>

            -   Mentions that peak-load prices for electricity, parking, etc are in practice.
                Why not use price to reduce queues?

            -   Supermarkets might collect a "check-out fee" that updates based on peak times and percentage of total bill.
                Maybe at some times we have a negative fee

            -   Customers would change their behavior to what time+checkout fee is best for them

        -   Saaty, T. L., & Leeman, W. A.
            (1965).
            The Burdens of Queuing Charges-Comments on a Letter by Leeman.
            Operations Research, 13(4), 679–681.
            <http://www.jstor.org/stable/167860>

            -   Charges for queues are ok for luxuries, but for needs like food this seems socially unjust

                -   Wowee what a sentence: "We should not then penalize the economically underprivileged who, caught in the maelstrom of modern life, must use these systems to obtain basic needs and who as a consequence of the dictates of demanding jobs cannot regulate their time to suit the pleasures and whims of those who have the resources and the leisure to obtain what they want when they want it."

            -   Not just socially unjust, but might have unexpected ripples on different scheduling issues

            -   "Some have argued that the man ahead in a queue imposes a cost on those who wait behind him and must pay for it. But I say that he has already paid for it by coming earlier."

            -   "I doubt its effectiveness for essential queues without the use of other substantial modifications"

        -   Leeman, W. A.
            (1965).
            ‘Comments’ on Saaty’s ‘The Burdens of Queuing Charges’.
            Operations Research, 13(4), 680-681.
            <http://www.jstor.org/stable/167860> (*Note - same link as Saaty, Leeman's comments are at the end of the PDF)*

            -   "...valid point..." but goes too far on the social justice, implies any price change is socially unjust

-   Charging for queues is similar to tolls on roads and we see that tolls help redistribute traffic since individuals will not

    -   Needed because it's been argued that **individual drivers, who focus on optimizing their route, will not optimize the overall system**.
        -   From a lecture by Professor Martin Beckmann (of Brown University and Bonn University) at Colloquium on "Decision Making in Traffic Planning" (organized in summer 1965 by Professor Arne Jensen of the Technical University, Copenhagen)

## Conditions for model

Similarly to drivers (who operating in their own self interest don't optimize public good), those queuing by optimizing their own needs won't optimize the overall system.
**IF the below conditions are met**

**1.** There exists a public good that can be maximized via objective func - "the expected overall profit (in unit time) accruing to arriving customers is a proper objective function representing public good."

    -   This can be centralized (e.g. public transportation or govt provided healthcare) and the decision maker is optimizing the system holistically to maximize public good.
    -   This can be decentralized, each customer acts for themselves to maximize their own utility. We can combine their utilities to maximize public good.

**2.** Customers are liable to be diverted from the service station - some will be directed not to queue, not invest time, and not reap benefits of the service they wanted to queue for

    -   Big contrast to assumption that the mission of the station is to serve *all* customers.
    That assumption leads to ordering customers by value to cut losses in case we can't serve everyone.

    -   This model will assume the only decision maker choice is to admit to line or not admit to line...
    a toll can prevent admission if $\text{utility of service} < \text{time} + \text{toll}$

## Overview of model

-   Arriving customer chooses one of two options by comparing utility of both (in the case of a tie we'll assume they join queue)

    1.  Joins queue $\to$ receives $R$eward and incurs $C$ost
    2.  Doesn't join queue $\to$ no $R$eward and no $C$ost

-   Model does not make an assumption that was usually made that we have steady state conditions (i.e. service time \> arrival time), because if arrivals out pace service then customers will not queue.

-   Model assumes that in "favorable circumstances" (e.g. short line) a customer will queue this is denoted by $v_s = \frac{R\mu}{C} \geq 1$ (in English: the reward is at least worth the cost of one service time).
    If this ratio isn't at least one we should just close because juice not worth squeeze for anyone any time.

    -   $C\mu^{-1}$ - $C$ost multiplied by expected service time - this is the expected loss of *the first* customer queuing (i.e. no wait in queue, only $C$ost is via time of being serviced)

    -   Getting to that ratio: $\frac{\text{reward}}{\text{cost per single service time}} = \frac{R}{C\mu^{-1}} = \frac{R\mu}{C}$

-   Variables and their meanings

    -   $\lambda$ - arrival rate of customers (poisson distributed; e.g. $\lambda = 5$ might be five customers arrive per hour)

    -   $\mu$ - service time intensity parameter (exponentially distributed; e.g. $\mu = 5$ might be a service can be completed every $\frac{1}{\mu} = \frac{1}{5} = 0.2$ hours

        -   NOTE: this is usually denoted $\lambda$ in exponential CDF/PDF/ETC, but $\lambda$ is already taking by customer arrival rate dist

    -   $R$ - customer $R$eward (in monetary units) for being serviced; all customer's $R$s are equal

    -   $C$ - the $C$ost (in monetary untis) *per unit of time* of a customer for queuing; all customer's $C$s are equal (e.g. $C$ might be $\$10$ *an hour*)

    -   $i$ - queue size at a given time (random var)

    -   $n$ - a constant denoting when Cost is too high and customer will not join queue; in the model $i < n$

    -   $\rho = \frac{\lambda}{\mu}$ - "utilization factor" - ratio of arrival rate to service rate - how busy service station is

        -   e.g. 5 customers arrive per hour ( $\lambda=5$ ) and we can service 10 customers per hour ( $\mu=10$ ) $\to$ $\rho = \frac{5}{10} = 0.5$; 50% of service capacity is being utilized

    -   Steady state equations - Steady state is where key metrics remain constant - i.e. avg length of queue, avg waiting time, system utilization.
        We must have service rate \< 1 (i.e. we have a service rate that can handle incoming arrival rate) OR some mechanism (balking, finite capacity, tolls).

        -   $p_i \rho = p_{i+1}$ - prob of $i$ customers in queue times utilization factor ( $\rho$ ) equals prob of $i+i$ customers in queue.
            $0\leq i < n$ - length of line is non-negative and always "worth it" to wait (if it's not worth it to wait the customer wouldn't have joined queue so we're bounded by customers' self-interest not to wait for less value than wait time costs them)

            -   Intuition - If we are busier (i.e. $\rho$ is large) we are more likely for queue to grow. If we are not busy (i.e. $\rho$ is small) the queue is less likely to grow because we process customers out of the queue.

        -   The solution of the above is: $p_i = \frac{\rho^i}{1+\rho^1+...+\rho^n}=\frac{\rho^i(1-\rho)}{1-\rho^{n+1}}$.

            -   Steps to derive this from $p_{i+1} = p_i \rho$

                -   Step 1 - solve for $p_i$ (go from $p_{i+1} = p_i \rho$ to $p_i=p_0 \rho^{i}$)

                    -   For $i=0$ $\to$ $p_1=p_0 \rho$

                    -   For $i=1$ $\to$ $p_2=p_1 \rho = (p_0 \rho)\rho = p_0\rho^2$

                    -   For $i=2$ $\to$ $p_3=p_2 \rho = (p_0 \rho^2)\rho = p_0\rho^3$

                    -   It follow, for any $p_i$ $\to$ $p_i=p_0 \rho^{i}$

                -   Step 2 - ensure probabilities sum to 1 (go from $p_i = p_0\rho^1$ to $p_i = \frac{\rho^i(1-\rho)}{1-\rho^{n+1}}$)

                    -   We need this: $\sum_{i=0}^np_i = 1$

                    -   Substitute in $p_i = p_0\rho^i$ so we get $\sum_{i=0}^np_0\rho^i = p_0(1 + \rho + \rho^2 + ...+\rho^n) =1$

                    -   Rewrite the sum of the geometric series so we get $p_0 \cdot \frac{1-\rho^{n + 1}}{1 - \rho} =1$

                        -   $1+ρ+\rho^2+...+\rho^n = \frac{1-\rho^{n + 1}}{1 - \rho}$ (assuming $\rho \neq 1$)

                        -   Because the sum of geometric series with first term $\alpha$, common ratio $r$, and $n+1$ terms the sum is given by: $S = \alpha \frac{1-r^{n+1}}{1-r}$

                            -   Here $\alpha=1$ & $r=\rho$

                    -   Solve for $p_0 = \frac{1-\rho}{1-\rho^{n+1}}$

                    -   Plug $p_0$ into formula for $p_i = p_0\rho^i = (\frac{1-\rho}{1-\rho^{n+1}})\rho^i$

                    -   Rewrite to match paper $\frac{\rho^i(1-\rho)}{1-\rho^{n+1}}$

        -   PGF is $g(z) = \sum_{i=0}^n p_iz^i = \frac{1-\rho}{1-\rho^{n+1}} \cdot \frac{1-(\rho z)^{n+1}}{1-\rho z}$

            -   Substitute in $p_i = \frac{(1 - \rho) \rho^i}{1 - \rho^{n+1}}$ in $\sum_{i=0}^n p_iz^i$
            -   This leads to $g(z) = \sum_{i=0}^n \frac{(1 - \rho) \rho^i}{1 - \rho^{n+1}} z^i = \frac{1 - \rho}{1 - \rho^{n+1}} \sum_{i=0}^n (\rho z)^i$
            -   Rewrite sum of geometric series $\sum_{i=0}^n (\rho z)^i$ as $\frac{1 - (\rho z)^{n+1}}{1 - \rho z}$ (assuming $\rho z \neq 1$)
            -   This leads to final form of $g(z) = \frac{1-\rho}{1-\rho^{n+1}} \cdot \frac{1-(\rho z)^{n+1}}{1-\rho z}$

        -   The expected value of $i$ (aka expected length of the queue) $E[i] = \frac{\rho[1-(n+1) \rho^n + n \rho ^ {n + 1}]}{(1-\rho)(1-\rho^{n+1})} = \frac{\rho}{1-\rho} - \frac{(n + 1)\rho^{n+1}}{1-\rho^{n+1}}$

            -   A property of PDFs (let's call it $g(z)$ is that $E[i] = g'(1)$

                -   Our PGF - $g(z) = \sum_{i=0}^{\infty} p_i z^i$
                -   It's 1st derivative - $g'(z) = \sum_{i=1}^{\infty} i p_i z^{i-1}$
                -   Plugging in 1 - $g'(1) = \sum_{i=1}^{\infty} i p_i \cdot 1^{i-1} = \sum_{i=1}^{\infty} i p_i = E[X]$
                    -   Note the term $ip_i$ is contribution of each value weighted by it's probability (aka expected value)

            -   Our $g(z) = \frac{1-\rho}{1-\rho^{n+1}} \cdot \frac{1-(\rho z)^{n+1}}{1-\rho z}$ and this leads to $g'(1) = E[i] = \frac{\rho}{1-\rho} - \frac{(n + 1)\rho^{n+1}}{1-\rho^{n+1}}$

        -   $\zeta = \lambda p_n = \frac{\lambda \rho ^ n (1-\rho)}{1-\rho^{n+1}}$ - arrival rate multiplied by prob of line being at capacity - represents customers diverted from service station

        -   This is a case where $\rho = \frac{\lambda}{\mu}$ is not the degree of utilization!!
            Instead we use $b = \sum_{i=1}^n p_i = 1 - p_0 = \frac{\rho (1 - \rho^n)}{1 - \rho^{n+1}}$

            -   This shows proportion of time service station is busy, but excludes the case of having no one in line and takes into account finite queue length $n$

        -   $\mu b = \mu (1-p_0)$ - Expected number of customers leaving service station - service intensity multiplied by fraction of time service is being rendered

The optimal policies and their derivations are not included in these math-ier notes, but they are included via code in the app.
